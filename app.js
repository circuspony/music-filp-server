const swipl = require('swipl');
// Engine represents one SWI-Prolog process.
const engine = new swipl.Engine();
(async () => {
    const result = await engine.call('consult(server)');
    // Either run more queries or stop the engine.
})().catch((err) => console.log(err));