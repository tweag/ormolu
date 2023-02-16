import { WASI } from "@bjorn3/browser_wasi_shim/src";
import ormoluWasm from "url:./ormolu.wasm";

async function run() {
    const wasi = new WASI([], [], []);
    const wasiImportObj = { wasi_snapshot_preview1: wasi.wasiImport };
    const wasm = await WebAssembly.instantiateStreaming(fetch(ormoluWasm), wasiImportObj);
    wasi.inst = wasm.instance;
    const exports = wasm.instance.exports;
    const memory = exports.memory;
    const encoder = new TextEncoder();
    const decoder = new TextDecoder();

    const outputPtrPtr = exports.mallocPtr();
    console.log("Initialized WASI reactor.");

    self.onmessage = event => {
        const input = event.data;
        const inputLen = Buffer.byteLength(input);
        const inputPtr = exports.malloc(inputLen);
        const inputArr = new Uint8Array(memory.buffer, inputPtr, inputLen);
        encoder.encodeInto(input, inputArr);
        const outputLen = exports.formatRaw(inputPtr, inputLen, outputPtrPtr);
        const outputPtrArr = new Uint32Array(memory.buffer, outputPtrPtr, 1);
        const outputPtr = outputPtrArr[0];
        const outputArr = new Uint8Array(memory.buffer, outputPtr, outputLen);
        const output = decoder.decode(outputArr);
        self.postMessage(output);
        exports.free(outputPtr);
    };

    self.postMessage(42);
}

run();
