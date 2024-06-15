'use strict';

const vueAppRootContainer = document.createElement("div");
vueAppRootContainer.id = "__nuxt";
vueAppRootContainer.setAttribute("hidden", "true");
document.body.appendChild(vueAppRootContainer);
const logger = console;
async function nuxtAppEntry() {
  const nuxtApp = () => import('#app/entry').then((m) => m.default).catch(() => {
  });
  const vueAppPromise = nuxtApp().catch((_error) => {
  });
  return vueAppPromise;
}
nuxtAppEntry().then((app) => {
  logger.log("nuxtAppEntry done", app);
  app().then(() => {
    logger.log("nuxtAppEntry app done");
  }).catch(() => {
    logger.log("nuxtAppEntry app error");
  });
});

module.exports = nuxtAppEntry;
