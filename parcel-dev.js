import { mainDev } from './output/Main/index.js';

let cancellationCallback;
function main() {
  cancellationCallback = mainDev();
}

// HMR setup. For more info see: https://parceljs.org/hmr.html
if (module.hot) {
  module.hot.accept(function () {
    console.log('Reloaded, running main again');
    cancellationCallback();
    console.log("ran cancellation")
    main();
  });
}

console.log('Starting app');
main();
