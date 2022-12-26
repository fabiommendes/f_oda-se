import { Elm } from '../elm/Main.elm';

// Start the Elm application.
let app = Elm.Main.init({
    node: document.querySelector('#app'),
});


// Start service workers
(async () => {
    if ("serviceWorker" in navigator) {
        // App shell SW 
        try {
            const registration = await navigator.serviceWorker.register("./sw.js", { scope: "/f_oda-se/" });
            if (registration.installing) {
                console.log("App shell SW installing");
            } else if (registration.waiting) {
                console.log("App shell SW installed");
            } else if (registration.active) {
                console.log("App shell SW active");
            }
        } catch (error) {
            console.error(`Registration failed with ${error}`);
        }
    }
})();