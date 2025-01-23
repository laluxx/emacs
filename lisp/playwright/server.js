const express = require('express');
const { chromium } = require('playwright');
const bodyParser = require('body-parser');

const app = express();
const port = 3000;

let browser = null;
let page = null;

app.use(bodyParser.json());

async function initBrowser() {
    if (!browser) {
        browser = await chromium.launch();
        page = await browser.newPage();
        console.log('Opening new page...');
        await page.goto('https://google.com');
        console.log('Page loaded successfully');
    }
}

app.post('/execute', async (req, res) => {
    try {
        await initBrowser();
        const { command } = req.body;
        
        // Execute the command and get the result
        const result = await page.evaluate((cmd) => {
            // Execute command in browser context
            try {
                const result = eval(cmd);
                return String(result);
            } catch (error) {
                return `Error: ${error.message}`;
            }
        }, command);

        res.json({ result });
    } catch (error) {
        res.status(500).json({ error: error.message });
    }
});

app.listen(port, () => {
    console.log(`Playwright server listening at http://localhost:${port}`);
});

process.on('SIGINT', async () => {
    if (browser) {
        await browser.close();
    }
    process.exit();
});
