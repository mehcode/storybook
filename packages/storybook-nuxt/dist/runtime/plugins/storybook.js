import { createNuxtApp, defineNuxtPlugin } from "nuxt/app";
import { getContext } from "unctx";
import { createRouter, createWebHistory } from "vue-router";
import plugins from "#build/plugins";
import "#build/css";
const globalWindow = window;
const logger = console;
export default defineNuxtPlugin({
  name: "storybook-nuxt-plugin",
  enforce: "pre",
  // or 'post'
  setup(nuxtApp) {
    logger.log("\u{1F50C} \u{1F50C} \u{1F50C}  [storybook-nuxt-plugin] setup ", { nuxtApp });
    const nuxtMainApp = getContext("nuxt-app");
    if (nuxtMainApp)
      logger.info(
        "\u{1F50C}  [storybook-nuxt-plugin] setup already done ",
        nuxtMainApp
      );
    if (nuxtApp.globalName !== "nuxt")
      return;
    const applyNuxtPlugins = async (vueApp, storyContext) => {
      const nuxt = createNuxtApp({
        vueApp,
        globalName: `nuxt-${storyContext.id}`
      });
      getContext("nuxt-app").set(nuxt, true);
      const router = nuxtApp.$router ?? createRouter({ history: createWebHistory(), routes: [] });
      nuxt.$router = router;
      getContext(nuxt.globalName).set(nuxt, true);
      nuxt.hooks.callHook("app:created", vueApp);
      for (const plugin of plugins) {
        try {
          if (typeof plugin === "function" && !plugin.toString().includes("definePayloadReviver"))
            await vueApp.runWithContext(() => plugin(nuxt));
        } catch (e) {
          logger.error("Error in plugin ", plugin);
        }
      }
      return nuxt;
    };
    globalWindow.PLUGINS_SETUP_FUNCTIONS ||= /* @__PURE__ */ new Set();
    globalWindow.PLUGINS_SETUP_FUNCTIONS.add(applyNuxtPlugins);
  },
  hooks: {
    "app:created": function() {
    }
  }
});
