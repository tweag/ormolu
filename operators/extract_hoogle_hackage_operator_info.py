import click
import os
import re
import bs4

OUTPUT_FILE_PATH = "src/Ormolu/Printer/HoogleHackageOperatorInfo.hs"
DEFAULT_FIXITY_DECL = ("infixl", 9)

SYMBOL_OP_REC = re.compile(r"^\s*?(\([^)]+?\))\s*?::.*$", re.MULTILINE)
FIXITY_REC = re.compile(r"^\s*?(infix[rl]?)\s+?([0-9])\s+?([^\s]+)\s*$", re.MULTILINE)

MODULE_HEADER = """module Ormolu.Printer.HoogleHackageOperatorInfo (packageToPopularity, packageToOps) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
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


def process_file(op_db, hoogle_database_path, file_path, conflict_map):
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
        op_db.on_decl(source, parse_decl_match(decl_match))
    for fixity_match in FIXITY_REC.finditer(content):
        op_db.on_fixity(source, *parse_fixity_match(fixity_match), conflict_map)


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
        name = columns[0].get_text().split(" ")[0].strip()
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


def op_info(fixity_decls):
    fix = lambda rawFix: {"infixl": "InfixL", "infixr": "InfixR", "infix": "InfixN"}[rawFix]
    return " <> ".join([f"FixityInfo {{fixDir = Just {fix(rawFix)}, fixMinPrec = {rawPrec}, fixMaxPrec = {rawPrec}}}" for rawFix, rawPrec in fixity_decls])

def dec_to_infix_name(dec_name):
    if len(dec_name) >= 2 and dec_name[0] == "(" and dec_name[-1] == ")":
        return dec_name[1:-1]
    else:
        return f"`{dec_name}`"
class OperatorDatabase:
    def __init__(self):
        self.packages = {}
    
    def on_decl(self, package_name, dec_name):
        if package_name not in self.packages:
            self.packages[package_name] = {}
            if package_name == "base":
                # Add ":" to base, the first time we encounter "base"
                self.packages[package_name][":"] = [("infixr", 5)]
        package = self.packages[package_name]
        infix_name = dec_to_infix_name(dec_name)
        if infix_name not in package:
            package[infix_name] = None
    
    def on_fixity(self, package_name, fixity_decl, infix_name, conflict_map):
        if package_name not in self.packages:
            self.packages[package_name] = {}
            if package_name == "base":
                # Add ":" to base, the first time we encounter "base"
                self.packages[package_name][":"] = [("infixr", 5)]
        package = self.packages[package_name]
        if infix_name not in package or package[infix_name] is None:
            package[infix_name] = [fixity_decl]
        elif fixity_decl in package[infix_name]:
            pass
        else:
            package[infix_name].append(fixity_decl)
            conflict_map[(package_name, infix_name)] = package[infix_name]

    def finalize(self):
        for package_name in self.packages:
            package = self.packages[package_name]
            for infix_name in package:
                if package[infix_name] is None:
                    package[infix_name] = [DEFAULT_FIXITY_DECL]
            self.packages[package_name] = sorted(list(package.items()), key=lambda x: (len(x[0]), x[0]))

def pop_lines(pop_db):
    lines = [(f"(\"{package_name}\", {pop}),") for (package_name, pop) in pop_db]
    lines[-1] = lines[-1][:-1]
    return (
        [
            "packageToPopularity :: HashMap String Int",
            "packageToPopularity =",
        ]
        + indent_lines(
            ["HashMap.fromList"]
            + indent_lines(
                ["[ " + lines[0]]
                + indent_lines(lines[1:])
                + ["]"]
            )
        )
    )

def sub_op_lines(package_name, ops):
    lines = [f"({op_name(op)}, {op_info(fixity_decls)})," for (op, fixity_decls) in ops]
    lines[-1] = lines[-1][:-1]
    return (
        [f"( \"{package_name}\",",]
        + indent_lines(
            ["HashMap.fromList"]
            + indent_lines(
                ["[ " + lines[0]]
                + indent_lines(lines[1:])
                + ["]"]
            )
        )
        + ["),"]
    )

def op_lines(op_db):
    lines = [subline for package_name, ops in op_db for subline in sub_op_lines(package_name, ops)]
    lines[-1] = lines[-1][:-1]
    return (
        [
            "packageToOps :: HashMap String (HashMap String FixityInfo)",
            "packageToOps =",
        ]
        + indent_lines(
            ["HashMap.fromList"]
            + indent_lines(
                ["[ " + lines[0]]
                + indent_lines(lines[1:])
                + ["]"]
            )
        )
    )

def conflict_lines(package_name, op_name, decls):
    return [f"{op_name} in {package_name}:"] + indent_lines([f"{dir} {prec}" for dir, prec in decls])

@click.command("operators", help=(
    "Extract operator information from the hoogle database"
))
@click.argument("HOOGLE_DATABASE_PATH", type=click.Path(file_okay=False, dir_okay=True, readable=True, resolve_path=True, exists=True))
@click.argument("HACKAGE_DATABASE_PATH", type=click.Path(file_okay=True, dir_okay=False, readable=True, resolve_path=True, exists=True))
@click.option("-d", "--debug-limit", type=int, help="limit the number of items in the output", default=None)
def main(hoogle_database_path, hackage_database_path, debug_limit):
    n = 0
    pop_db = load_hackage_db(hackage_database_path)
    op_db = OperatorDatabase()
    to_explore = [hoogle_database_path]
    conflict_map = {}
    while to_explore:
        new_to_explore = []
        for path in to_explore:
            for root, dirs, files in os.walk(path):
                new_to_explore.extend(os.path.join(root, dir) for dir in dirs)
                for file_name in files:
                    file_path = os.path.join(root, file_name)
                    process_file(op_db, hoogle_database_path, file_path, conflict_map)
                    n += 1
        to_explore = new_to_explore
    conflict_map = sorted(list(conflict_map.items()), key=lambda x: x[0])
    op_db.finalize()
    print(f"{n} files processed!")
    pop_db = sorted(list(pop_db.items()), key=lambda x: x[0])
    pop_n = len(pop_db)
    print(f"Found popularity information for {pop_n} packages")
    op_db = sorted(list(op_db.packages.items()), key=lambda x: x[0])
    op_n = len(op_db)
    distinct_ops = set()
    k = 0
    for _, ops in op_db:
        k += len(ops)
        distinct_ops.update(name for name, _ in ops)
    print(f"Found {k} operator declarations across {op_n} packages for a total of {len(distinct_ops)} distinct operators.")
    if conflict_map:
        print(
            f"Found {len(conflict_map)} conflicts within packages themselves:\n" +
            "\n".join(indent_lines([
                line
                for ((package_name, op_name), decls) in conflict_map
                for line in conflict_lines(package_name, op_name, decls)
            ]))
        )
    if debug_limit:
        pop_db = pop_db[:min(pop_n, debug_limit)]
        op_db = op_db[:min(op_n, debug_limit)]
    with open(OUTPUT_FILE_PATH, "w", encoding="utf-8") as output_file:
        output_file.write(MODULE_HEADER)
        output_file.write("\n".join(pop_lines(pop_db)))
        output_file.write("\n\n")
        output_file.write("\n".join(op_lines(op_db)))
        output_file.write("\n")


if __name__ == "__main__":
    main()
