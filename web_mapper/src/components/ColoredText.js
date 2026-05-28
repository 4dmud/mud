import React from 'react';
import { parseColorCodes } from '../utils/colorParser';

const ColoredText = ({ text }) => {
  const parts = parseColorCodes(text);
  return (
    <span>
      {parts.map((part, index) => (
        <span key={index} style={{ color: part.color }}>
          {part.text}
        </span>
      ))}
    </span>
  );
};

export default ColoredText;
