"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const fs_1 = __importDefault(require("fs"));
const path_1 = __importDefault(require("path"));
const mkdirp_1 = __importDefault(require("mkdirp"));
const fflate_1 = require("fflate");
async function unzip(zipfile, opts) {
    var { to: { directory: odir, fs: ofs } } = options(opts), z = fflate_1.unzipSync(await open(zipfile));
    for (let [relativePath, content] of Object.entries(z)) {
        var outf = path_1.default.join(odir, relativePath);
        mkdirp_1.default.sync(path_1.default.dirname(outf), { fs: ofs });
        ofs.writeFileSync(outf, content);
    }
}
async function open(zipfile) {
    if (typeof zipfile === 'string') {
        return fs_1.default.readFileSync ? fs_1.default.readFileSync(zipfile)
            : new Uint8Array(await (await fetch(zipfile)).arrayBuffer());
    }
    else if (zipfile instanceof ArrayBuffer) {
        return new Uint8Array(zipfile);
    }
    else
        return zipfile;
}
function options(opts) {
    if (opts) {
        if (typeof opts === 'string') {
            return { to: { directory: opts, fs: fs_1.default } };
        }
        else if (opts.to) {
            if (typeof opts.to === 'string')
                return { to: { directory: opts.to, fs: fs_1.default } };
            else
                return { to: { directory: opts.to.directory || '',
                        fs: opts.to.fs || fs_1.default } };
        }
    }
    return { to: { directory: '', fs: fs_1.default } };
}
exports.default = unzip;
