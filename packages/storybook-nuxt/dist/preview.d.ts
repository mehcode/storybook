import * as vue from 'vue';
import * as nuxt_dist_app from 'nuxt/dist/app';

declare function nuxtAppEntry(): Promise<void | ((ssrContext?: nuxt_dist_app.NuxtSSRContext | undefined) => Promise<vue.App<Element>>)>;

export { nuxtAppEntry as default };
