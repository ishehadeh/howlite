import {
    flavors
} from "https://deno.land/x/catppuccin@v1.7.1/mod.ts";

let fileContent = "\n#let colors = (\n"
flavors["latte"].colorEntries.forEach(([colorName, { hex }]) => {
    fileContent += `    ${colorName}: rgb("${hex}"),\n`
})

fileContent += ");\n\n";
fileContent += "#let nth-color(n) = colors.values().at(calc.rem(n, 12));"


Deno.writeTextFileSync(import.meta.dirname! + "/../templates/colors.typ", fileContent);