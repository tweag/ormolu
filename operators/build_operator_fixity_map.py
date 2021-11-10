import click
import os
import re
import bs4

OUTPUT_FILE_PATH = "src/Ormolu/Printer/OperatorFixityMap.hs"
DEFAULT_FIXITY_DECL = ("infixl", 9)

SYMBOL_OP_REC = re.compile(r"^\s*?(\([^)]+?\))\s*?::.*$", re.MULTILINE)
FIXITY_REC = re.compile(r"^\s*?(infix[rl]?)\s+?([0-9])\s+?([^\s]+)\s*$", re.MULTILINE)

top_level_content = """module Ormolu.Printer.OperatorFixityMap (defaultFixityMap) where

import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Types.Basic (FixityDirection (..))
import Ormolu.Printer.FixityInfo (FixityInfo (..))

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


def load_hackage_db(db_path):
    with open(db_path, "r", encoding="utf-8") as db_file:
        content = db_file.read()
    soup = bs4.BeautifulSoup(content, "lxml")
    table = soup.find_all("tbody")[0]
    db = {}
    for table_row in table.find_all("tr"):
        columns = table_row.find_all("td")
        if len(columns) < 2:
            continue
        name = columns[0].get_text().strip()
        dl_count = int(columns[1].get_text().strip())
        if name in db:
            raise Exception(f"{name} already present")
        db[name] = dl_count
    return db


def parse_decl_match(match):
    # dec_name
    return match.group(1)


def parse_fixity_match(match):
    # fixity_decl, infix_name
    return ((match.group(1), int(match.group(2))), match.group(3))


def indent_lines(lines):
    return ["  " + l for l in lines]


def op_name(infix_name):
    return "\"" + infix_name.strip("`").replace("\\", "\\\\") + "\""


def op_info(rawFix, rawPrec):
    fix = {"infixl": "InfixL", "infixr": "InfixR", "infix": "InfixN"}[rawFix]
    return f"FixityInfo {{fixDir = Just {fix}, fixMinPrec = {rawPrec}, fixMaxPrec = {rawPrec}}}"

def rank_source(source_tuple):
    (source, is_base, pop) = source_tuple
    return (1 if is_base else 0, 0 if pop is None else pop, source)

def rank_fixity(fixity_tuple):
    (fix_decl, (has_base, fix_pop, sources)) = fixity_tuple
    return (1 if has_base else 0, fix_pop)

def rank_operator(op_tuple):
    (name, (in_base, op_pop, fixities)) = op_tuple
    return (1 / len(name) if in_base else 0, op_pop)

def score(rel, tot):
    if rel is None:
        return f"?/{tot} = ?%"
    if tot == 0:
        assert rel == 0
        return f"0/0 = ?%"
    percentage = rel / tot * 100
    return f"{rel}/{tot} = {percentage:.1f}%"

def origin_comment(fix, prec, fix_pop, op_pop, sources):
    return (
        f"-- {fix} {prec} ({score(fix_pop, op_pop)}) in " +
        ", ".join([f"{source} ({score(pop, op_pop)})" for (source, is_base, pop) in sources])
    )

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
    
    def __len__(self):
        return len(self.operators)

    def output(self, output_file_path, hackage_db, threshold, debug_limit=None):
        not_found_sources = set()
        new_operators = [(":", (True, 0, [(("infixr", 5), (True, 0, [("<compiler>", True, None)]))]))]
        for operator in self.operators.values():
            operator.resolve_default_fixities()
            new_fixities = []
            op_pop = 0
            in_base = False
            for fix_decl, sources in operator.fixities.items():
                has_base = False
                fix_pop = 0
                new_sources = []
                for source in sources:
                    pop = hackage_db.get(source)
                    if pop is not None:
                        fix_pop += pop
                        op_pop += pop
                    else:
                        not_found_sources.add(source)
                    new_sources.append((source, source == "base", pop))
                    has_base = has_base or source == "base"
                new_sources.sort(key=rank_source, reverse=True)
                new_fixities.append((fix_decl, (has_base, fix_pop, new_sources)))
                in_base = in_base or has_base
            new_fixities.sort(key=rank_fixity, reverse=True)
            new_operators.append((operator.infix_name, (in_base, op_pop, new_fixities)))
        new_operators.sort(key=rank_operator, reverse=True)

        n = len(new_operators)
        not_found_sources = sorted(list(not_found_sources))
        print(f"Found {n} operators!")
        print(f"Sources with no DL count available: (n = {len(not_found_sources)}) {not_found_sources}")

        if debug_limit is not None:
            new_operators = new_operators[:min(n, debug_limit)]

        n = len(new_operators)
        map_items = []
        for i, (name, (in_base, op_pop, fixities)) in enumerate(new_operators):
            comment_value_lines = []
            is_first = True
            stop_message = None
            for ((fix, prec), (has_base, fix_pop, sources)) in fixities:

                comment_value_lines.append(origin_comment(fix, prec, fix_pop, op_pop, sources))
                if is_first:
                    is_first = False
                    value_line = op_info(fix, prec)
                    if has_base:
                        stop_message = "conflict with base"
                    elif op_pop > 0 and fix_pop / op_pop >= threshold:
                        stop_message = "threshold reached"
                else:
                    value_line = ("  <> " if stop_message is None else f"--  ({stop_message}) <> ") + op_info(fix, prec)
                comment_value_lines.append(value_line)


            
            map_item = ["( " + op_name(name) + ","] + indent_lines(comment_value_lines) + [")"]
            if i < n - 1:
                map_item[-1] += ","
            map_items.extend(map_item)
        
        map_content = ["defaultFixityMap :: Map String FixityInfo", "defaultFixityMap ="] + indent_lines(["Map.fromList"] + indent_lines(["[ " + map_items[0]] + indent_lines(map_items[1:]) + ["]"]))

        with open(output_file_path, "w", encoding="utf-8") as output_file:
            output_file.write(top_level_content)
            for line in map_content:
                output_file.write(line + "\n")


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
            self.fixities[DEFAULT_FIXITY_DECL] = self.fixities[DEFAULT_FIXITY_DECL].union(sources_with_default_fixity)
        assert len(self.fixities) > 0
    
    def has_conflicting_fixities(self):
        return len(self.fixities) > 1


@click.command("operators", help=(
    "Extract operator information from the hoogle database"
))
@click.argument("HOOGLE_DATABASE_PATH", type=click.Path(file_okay=False, dir_okay=True, readable=True, resolve_path=True, exists=True))
@click.argument("HACKAGE_DATABASE_PATH", type=click.Path(file_okay=True, dir_okay=False, readable=True, resolve_path=True, exists=True))
@click.option("-d", "--debug-limit", type=int, help="limit the number of items in the output map to 2*n or less", default=None)
@click.option("-t", "--threshold", type=float, help="popularity ratio (between 0 and 1) after which the most popular definition hides other definitions. With t == 0, only the most popular definition will be kept in case of conflict, and with t == 1 (the default value), all definitions will be kept. In any case, definitions from 'base' will hides other definitions", default=1.)
def main(hoogle_database_path, hackage_database_path, debug_limit, threshold):
    n = 0
    hackage_db = load_hackage_db(hackage_database_path)
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
    op_db.output(OUTPUT_FILE_PATH, hackage_db, threshold, debug_limit=debug_limit)


if __name__ == "__main__":
    main()
