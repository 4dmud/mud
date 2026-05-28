export const colorCodes = {
  'c0': '#FFFFFF', // Normal
  'ck': '#000000', // Black
  'cr': '#800000', // Red
  'cg': '#008000', // Green
  'cy': '#808000', // Yellow
  'cb': '#000080', // Blue
  'cm': '#800080', // Magenta
  'cc': '#008080', // Cyan
  'cw': '#C0C0C0', // White
  'cK': '#808080', // Bold Black (Grey)
  'cR': '#FF0000', // Bold Red
  'cG': '#00FF00', // Bold Green
  'cY': '#FFFF00', // Bold Yellow
  'cB': '#0000FF', // Bold Blue
  'cM': '#FF00FF', // Bold Magenta
  'cC': '#00FFFF', // Bold Cyan
  'cW': '#FFFFFF', // Bold White
};

export function parseColorCodes(text) {
  if (!text) return [];

  const parts = [];
  const regex = /\{([a-zA-Z0-9]{2})/g;
  let lastIndex = 0;
  let currentColor = colorCodes['c0'];
  let match;

  while ((match = regex.exec(text)) !== null) {
    const textPart = text.substring(lastIndex, match.index);
    if (textPart) {
      parts.push({ text: textPart, color: currentColor });
    }

    const code = match[0].substring(1);
    if (colorCodes[code]) {
      currentColor = colorCodes[code];
    }
    lastIndex = regex.lastIndex;
  }

  const remainingText = text.substring(lastIndex);
  if (remainingText) {
    parts.push({ text: remainingText, color: currentColor });
  }

  return parts;
}
