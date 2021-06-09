/// <reference types="node" />
import fs from 'fs';
export declare type UnzipSource = string | ArrayBuffer | Uint8Array;
export declare type UnzipOptions = {
    to?: {
        fs?: typeof fs;
        directory?: string;
    };
};
declare function unzip(zipfile: UnzipSource, opts?: UnzipOptions | string): Promise<void>;
export default unzip;
