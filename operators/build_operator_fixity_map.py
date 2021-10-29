import click
import os
import re
import csv
import shlex

OUTPUT_FILE_PATH = "src/Ormolu/Printer/OperatorFixityMap.hs"
DEFAULT_FIXITY_DECL = ("infixl", 9)

SYMBOL_OP_REC = re.compile(r"^\s*?(\([^)]+?\))\s*?::.*$", re.MULTILINE)
FIXITY_REC = re.compile(r"^\s*?(infix[rl]?)\s+?([0-9])\s+?([^\s]+)\s*$", re.MULTILINE)

top_level_content = """module Ormolu.Printer.OperatorFixityMap (fixityMap) where

import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Types.Basic ( FixityDirection (..) )

import Ormolu.Printer.FixityInfo
  ( FixityInfo (..),
  )

"""

def get_source(hoogle_database_path, file_path):
    assert file_path.startswith(hoogle_database_path)
    relative_path = file_path[len(hoogle_database_path):]
    result = relative_path.split("/")[1]
    if not result:
        raise Exception("Source is empty: " + result)
    return result


def process_file(op_db, hoogle_database_path, file_path):
    print(f"Processing {file_path}...")
    for encoding in ["utf-8", "latin1"]:
        try:
            with open(file_path, "r", encoding=encoding) as file:
                content = file.read()
            break
        except:
            print(f"Trying latin1 for {file_path} (utf-8 failed)...")
    source = get_source(hoogle_database_path, file_path)
    for decl_match in SYMBOL_OP_REC.finditer(content):
        op_db.on_decl(parse_decl_match(decl_match), source)
    for fixity_match in FIXITY_REC.finditer(content):
        op_db.on_fixity(*parse_fixity_match(fixity_match), source)


def parse_decl_match(match):
    # dec_name
    return match.group(1)


def parse_fixity_match(match):
    # fixity_decl, infix_name
    return ((match.group(1), int(match.group(2))), match.group(3))


def indent_lines(lines):
    return ["  " + l for l in lines]


def op_name(operator):
    return "\"" + operator.infix_name.strip("`").replace("\\", "\\\\") + "\""


def op_info(rawFix, rawPrec):
    fix = {"infixl": "InfixL", "infixr": "InfixR", "infix": "InfixN"}[rawFix]
    return f"FixityInfo {{fixDirection=Just {fix}, fixMinPrec={rawPrec}, fixMaxPrec={rawPrec}}}"


class OperatorDatabase:
    def __init__(self):
        self.operators = {}
    
    def on_decl(self, dec_name, source):
        if dec_name not in self.operators:
            self.operators[dec_name] = Operator(dec_name)
        self.operators[dec_name].add_source(source)
    
    def on_fixity(self, fixity_decl, infix_name, source):
        dec_name = Operator.infix_to_dec_name(infix_name)
        if dec_name not in self.operators:
            self.operators[dec_name] = Operator(dec_name)
        self.operators[dec_name].add_fixity(fixity_decl, source)
    
    def finalize(self):
        for operator in self.operators.values():
            operator.resolve_default_fixities()
    
    def __len__(self):
        return len(self.operators)

    def output_in(self, output_file_path, debug_limit=None):
        non_conflicting = []
        conflicting = []
        for operator in self.operators.values():
            if operator.has_conflicting_fixities():
                conflicting.append(operator)
            else:
                non_conflicting.append(operator)
        conflicting.sort(key=lambda op: op.infix_name)
        non_conflicting.sort(key=lambda op: op.infix_name)

        # Do not generate a file with 16k lines, because HLS is not able to index it in reasonable time
        if debug_limit is not None:
            c = len(conflicting)
            conflicting = conflicting[:min(c, debug_limit)]
            nc = len(conflicting)
            non_conflicting = non_conflicting[:min(nc, debug_limit)]

        operators = non_conflicting + conflicting

        map_items = []
        n = len(operators)
        for i, operator in enumerate(operators):
            comment_value_lines = []
            is_first = True
            for (fix, prec), sources in operator.fixities.items():
                comment_value_lines.append("-- {} {} in {}".format(fix, prec, ", ".join(sources)))
                if is_first:
                    is_first = False
                    value_line = op_info(fix, prec)
                else:
                    value_line = "  <> " + op_info(fix, prec)
                comment_value_lines.append(value_line)
            
            map_item = ["("] + indent_lines([op_name(operator) + ","] + comment_value_lines) + [")"]
            if i < n - 1:
                map_item[-1] += ","
            map_items.extend(map_item)
        map_content = ["fixityMap :: Map String FixityInfo", "fixityMap ="] + indent_lines(["Map.fromList ["] + indent_lines(map_items) + ["]"])

        with open(output_file_path, "w", encoding="utf-8") as output_file:
            output_file.write(top_level_content)
            for line in map_content:
                output_file.write(line + "\n")

        print(f"Found {len(non_conflicting)} non-conflicting and {len(conflicting)} conflicting operators.")

class Operator:
    @staticmethod
    def infix_to_dec_name(infix_name):
        if len(infix_name) >= 2 and infix_name[0] == "`" and infix_name[-1] == "`":
            return infix_name[1:-1]
        else:
            return f"({infix_name})"
    def dec_to_infix_name(dec_name):
        if len(dec_name) >= 2 and dec_name[0] == "(" and dec_name[-1] == ")":
            return dec_name[1:-1]
        else:
            return f"`{dec_name}`"

    def __init__(self, dec_name):
        self.dec_name = dec_name
        self.infix_name = Operator.dec_to_infix_name(self.dec_name)
        self.is_symbolic = len(self.dec_name) >= 2 and self.dec_name[0] == "(" and self.dec_name[-1] == ")"
        self.fixities = {}
        # TODO: assign default fixity in self.fixities when a source declares an operator but doesn't declare its fixity
        self.sources = set()
    
    def add_fixity(self, fixity_decl, source):
        if fixity_decl not in self.fixities:
            self.fixities[fixity_decl] = set()
        self.fixities[fixity_decl].add(source)
    
    def add_source(self, source):
        self.sources.add(source)
    
    def resolve_default_fixities(self):
        sources_with_specified_fixity = set(source for _, sources in self.fixities.items() for source in sources)
        sources_with_default_fixity = self.sources.difference(sources_with_specified_fixity)
        if sources_with_default_fixity:
            if DEFAULT_FIXITY_DECL not in self.fixities:
                self.fixities[DEFAULT_FIXITY_DECL] = set()
            self.fixities[DEFAULT_FIXITY_DECL].union(sources_with_default_fixity)
        assert len(self.fixities) > 0
    
    def has_conflicting_fixities(self):
        return len(self.fixities) > 1


@click.command("operators", help=(
    "Extract operator information from the hoogle database"
))
@click.argument("HOOGLE_DATABASE_PATH", type=click.Path(file_okay=False, dir_okay=True, readable=True, resolve_path=True, exists=True))
@click.option("-d", "--debug-limit", type=int, help="limit the number of items in the output map to 2*n or less", default=None)
def main(hoogle_database_path, debug_limit):
    n = 0
    op_db = OperatorDatabase()
    to_explore = [hoogle_database_path]
    while to_explore:
        new_to_explore = []
        for path in to_explore:
            for root, dirs, files in os.walk(path):
                new_to_explore.extend(os.path.join(root, dir) for dir in dirs)
                for file_name in files:
                    file_path = os.path.join(root, file_name)
                    process_file(op_db, hoogle_database_path, file_path)
                    n += 1
        to_explore = new_to_explore
    print(f"{n} files processed!")
    op_db.finalize()
    op_db.output_in(OUTPUT_FILE_PATH, debug_limit=debug_limit)


if __name__ == "__main__":
    main()
