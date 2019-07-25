/* eslint-disable no-loop-func */

import path from "path";
import fs from "fs-extra";
import Jimp from "jimp/es";
import mergeImg from "merge-img";

const sizeOf = require("image-size");

(async function () {
    let i = 0;
    let offset = 0;
    let screenshotPaths = [];
    let scrollHeight = Math.max(
        document.body.scrollHeight,
        document.body.offsetHeight,
        document.documentElement.clientHeight,
        document.documentElement.scrollHeight,
        document.documentElement.offsetHeight
    );
    let targetPath = path.join(
        nw.App.startPath,
        "final.png"
    );

    nw.Window.get().resizeTo(window.screen.width, window.screen.height);
    await new Promise(resolve => setTimeout(resolve, 100));

    while (offset < scrollHeight) {
        window.scrollTo(0, offset);
        await new Promise(resolve => setTimeout(resolve, 100));

        let screenshotPath = path.join(
            nw.App.startPath,
            `part${i++ + 1}.png`
        );

        await fs.ensureFile(screenshotPath);
        screenshotPaths.push(screenshotPath);

        let imgBuffer = await new Promise(resolve => {
            nw.Window.get().capturePage(resolve, { format: "png", datatype: "buffer" });
        });

        await fs.writeFile(screenshotPath, imgBuffer);
        offset += sizeOf(imgBuffer).height;

        if (scrollHeight - offset < 0) {
            await new Promise(resolve => {
                new Jimp(imgBuffer, function () {
                    this.crop(0, offset - scrollHeight, sizeOf(imgBuffer).width, sizeOf(imgBuffer).height - (offset - scrollHeight))
                        .write(screenshotPath, resolve);
                });
            });
        }
    }

    let targetImg = await mergeImg(screenshotPaths, { direction: true });
    targetImg.write(targetPath);
    screenshotPaths.forEach(async p => await fs.unlink(p));
})();
