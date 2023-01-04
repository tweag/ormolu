import Clipboard from "clipboard";
import meta from "../../src/meta.json";

export const metadata = meta;

export const spawnOrmoluWorker = init => onmessage => () => {
    const worker = new Worker(
        new URL('../../src/worker.js', import.meta.url),
        {type: 'module'}
    );
    worker.onmessage = e => {
        const r = e.data;
        if (r === 42) {
            init();
        } else {
            onmessage(JSON.parse(r))();
        }
    };
    return s => () => worker.postMessage(JSON.stringify(s));
};

export const newClipboard = el => getStr => () => {
    new Clipboard(el, { text: trigger => getStr() });
};
