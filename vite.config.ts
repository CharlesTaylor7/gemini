import path from 'node:path';
import { defineConfig } from 'vite';

export default defineConfig({
  base: '/gemini/',
  publicDir: 'static',
  server: {
    port: process.env.PORT,
  },
  optimizeDeps: {
    include: ['output/**/*.js'],
  },
});
