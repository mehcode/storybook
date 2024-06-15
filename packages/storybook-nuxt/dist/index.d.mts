export * from '@storybook/vue3';
export { DecoratorFunction, Meta, Preview, StoryFn, StoryObj, VueRenderer } from '@storybook/vue3';
import { BuilderOptions, StorybookConfig as StorybookConfig$1 } from '@storybook/types';

type FrameworkName = '@storybook-vue/nuxt'
type BuilderName = '@storybook/builder-vite'

type FrameworkOptions = NuxtOptions & {
  builder?: BuilderOptions
}

type StorybookConfigFramework = {
  framework: FrameworkName | { name: FrameworkName; options: FrameworkOptions }
  core?: StorybookConfig$1['core'] & { builder?: BuilderName }
  typescript?: StorybookConfig$1['typescript']
  previewAnnotations?: StorybookConfig$1['previewAnnotations']
  stories?: StorybookConfig$1['stories']
  addons?: StorybookConfig$1['addons']
  docs?: StorybookConfig$1['docs']
}
/**
 * The interface for Storybook configuration in `main.ts` files.
 */
type StorybookConfig = {
  viteFinal?: Record<string, unknown>
} & StorybookConfigFramework
interface NuxtOptions {}

export type { FrameworkOptions, NuxtOptions, StorybookConfig };
