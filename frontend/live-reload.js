import { main } from './Main/index.js';

new EventSource('/esbuild').addEventListener('change', () => location.reload());

main();
