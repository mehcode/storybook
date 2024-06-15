import { PresetProperty } from '@storybook/types';
import { Nuxt } from '@nuxt/schema';
import { StorybookConfig } from './index.mjs';
import '@storybook/vue3';

declare const core: PresetProperty<'core', StorybookConfig>;
/**
 *
 * @param entry preview entries
 * @returns preview entries with nuxt runtime
 */
declare const previewAnnotations: StorybookConfig['previewAnnotations'];
declare const viteFinal: StorybookConfig['viteFinal'];
declare function getNuxtProxyConfig(nuxt: Nuxt): {
    port: any;
    route: string;
    proxy: {
        "^/(_nuxt|_ipx|_icon|__nuxt_devtools__)": {
            target: string;
            changeOrigin: boolean;
            secure: boolean;
            ws: boolean;
        };
    };
};

export { core, getNuxtProxyConfig, previewAnnotations, viteFinal };
