require("ace-builds/src/ace.js");
require("ace-builds/src/mode-haskell.js");
require("ace-builds/src/theme-github_light_default.js");
require("ace-builds/src/theme-github_dark.js");

runJSaddle(new Worker("worker.js", { type: "module" }));
