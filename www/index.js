import * as wasm from "emu8085";

const handle = new wasm.WebHandle();
handle.start(document.getElementById("canvas"));
