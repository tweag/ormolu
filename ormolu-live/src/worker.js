import { WASI } from "@bjorn3/browser_wasi_shim/src";
import ormoluWasm from "url:./ormolu.wasm";
import hackageInfoBin from "url:./hackage-info.bin";

async function run() {
    const wasi = new WASI([], [], []);
    const wasiImportObj = { wasi_snapshot_preview1: wasi.wasiImport };
    const [wasm, hackageInfoBuf] = await Promise.all([
        WebAssembly.instantiateStreaming(fetch(ormoluWasm), wasiImportObj),
        fetch(hackageInfoBin).then(r => r.arrayBuffer()),
    ]);
    wasi.inst = wasm.instance;
    const exports = wasm.instance.exports;
    const memory = exports.memory;
    const encoder = new TextEncoder();
    const decoder = new TextDecoder();

    exports._initialize();
    exports.hs_init(0, 0);
    const hackageInfoLen = hackageInfoBuf.byteLength;
    const hackageInfoPtr = exports.malloc(hackageInfoLen);
    const hackageInfoArrSrc =
          new Uint8Array(hackageInfoBuf, 0, hackageInfoLen);
    const hackageInfoArrDst =
          new Uint8Array(memory.buffer, hackageInfoPtr, hackageInfoLen);
    hackageInfoArrDst.set(hackageInfoArrSrc);
    exports.initFixityDB(hackageInfoPtr, hackageInfoLen);
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
