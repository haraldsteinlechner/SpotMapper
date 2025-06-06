import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

export default defineConfig({
  plugins: [react()],
  root: "./src",
  publicDir: "../public",
  build: {
    sourcemap: true,
    outDir: "../docs",
  }
})
