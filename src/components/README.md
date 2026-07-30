# src/components

This directory contains custom Lazarus / Free Pascal visual components and a Lazarus package used by OvoPlayer. The components provide UI widgets and helper classes used across the application (sliders, image/icon helpers, registration unit, etc.).

## License

The project files in this repository are covered by the GNU General Public License (see project headers). Check the repository top-level LICENSE or source file headers for exact license text.

## Contents

- `mcaselli.lpk` — Lazarus package file containing the components in this folder.
- `mcaselli.pas` — Lightweight unit usually used by the package (helper/registration support).
- `registermcaselli.pas` — Unit that registers the components in the Lazarus component palette.

### Visual / helper components
- `fontimagelist.pas` — A FontImageList component: draw icons from fonts or font-based glyph sets into an image list for use in UI controls.
- `fontimagelisteditor.pas` / `fontimagelisteditor.lfm` — Editor form and Lazarus form file used to edit FontImageList entries inside the IDE.
- `iconrender.pas` — Helper unit to render icons (likely used by OSD, toolbar or lists).
- `imagetrack.pas` — Component for image tracking/display (used for cover art, animated images, or image lists tied to tracks)

### Sliders and skins
- `slider.pas` — Base/custom slider control used by the application.
- `skinnedslider.pas` — Skinned slider control with custom drawing and styling.
- `skinnedslider_icon.lrs` — Resource file used by the skinned slider (contains embedded images/icons used by the component).
- `themedslider.pas` — Themed slider variant that adapts to application theme or style.

### Subdirectories
- `network/` — (directory) network-related components; open that folder for network-specific components and units.

## How to install (Lazarus)

1. Open Lazarus IDE.
2. Open the package file `src/components/mcaselli.lpk` (Package → Open Package File (.lpk)).
3. Compile and then Install the package. Lazarus may prompt to rebuild the IDE.
4. After installation the components should appear in the component palette and can be dropped onto forms.

## How to use the components in code

- Add the component unit name to your uses clause. The package registers the components for the IDE, but you can also reference the units directly at compile time (for example: `uses mcaselli, fontimagelist, skinnedslider;`).
- Ensure resource files (like `.lrs`) are compiled into your project if a component depends on them (this typically happens automatically when the component/unit is included).

## Notes for maintainers

- Keep the package `mcaselli.lpk` synchronized with the list of units in this folder.
- `registermcaselli.pas` contains the component registration (component palette categories and class registration). Update it when adding or removing components.
- Resource files (e.g., `skinnedslider_icon.lrs`) store embedded images; use `lazres` to inspect or re-generate them when needed.

## Contributing

Contributions are welcome. Please open issues or pull requests on the main repository. Describe the component change, provide screenshots if it affects UI, and include minimal reproduction steps.

## Contact / Additional info

For details about how these components are used by OvoPlayer, inspect the `src/` code that references the units (for example, forms under `src` which use `mcaselli`, `fontimagelist` or the slider units).
