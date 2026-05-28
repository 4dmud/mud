import React, { useEffect, useRef } from 'react';

const MapCanvas = ({ rooms, coordinates, currentZ, selectedRoom, onRoomSelect }) => {
  const canvasRef = useRef(null);
  const zoom = 50;

  useEffect(() => {
    const canvas = canvasRef.current;
    if (!canvas) return;
    const ctx = canvas.getContext('2d');

    // Clear canvas
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    ctx.fillStyle = '#111';
    ctx.fillRect(0, 0, canvas.width, canvas.height);

    const filteredRooms = rooms.filter(r => coordinates[r.number] && coordinates[r.number].z === currentZ);

    if (filteredRooms.length === 0) return;

    // Find bounds to center
    let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity;
    filteredRooms.forEach(r => {
      const coord = coordinates[r.number];
      if (coord.x < minX) minX = coord.x;
      if (coord.y < minY) minY = coord.y;
      if (coord.x > maxX) maxX = coord.x;
      if (coord.y > maxY) maxY = coord.y;
    });

    const offsetX = (canvas.width / 2) - ((minX + maxX) / 2 * zoom);
    const offsetY = (canvas.height / 2) - ((minY + maxY) / 2 * zoom);

    // Draw lines first
    ctx.strokeStyle = '#444';
    ctx.lineWidth = 2;
    filteredRooms.forEach(room => {
      const coord = coordinates[room.number];
      room.exits.forEach(exit => {
        const destCoord = coordinates[exit.destination];
        if (destCoord && destCoord.z === currentZ) {
          ctx.beginPath();
          ctx.moveTo(coord.x * zoom + offsetX, coord.y * zoom + offsetY);
          ctx.lineTo(destCoord.x * zoom + offsetX, destCoord.y * zoom + offsetY);
          ctx.stroke();
        }
      });
    });

    // Draw rooms
    filteredRooms.forEach(room => {
      const coord = coordinates[room.number];
      const posX = coord.x * zoom + offsetX;
      const posY = coord.y * zoom + offsetY;

      ctx.fillStyle = room.number === (selectedRoom ? selectedRoom.number : null) ? '#ff0' : '#007bff';
      ctx.beginPath();
      ctx.arc(posX, posY, 10, 0, Math.PI * 2);
      ctx.fill();

      ctx.fillStyle = '#fff';
      ctx.font = '10px Arial';
      ctx.fillText(room.number, posX + 12, posY + 4);
    });

  }, [rooms, coordinates, currentZ, selectedRoom]);

  const handleClick = (e) => {
    const rect = canvasRef.current.getBoundingClientRect();
    const mouseX = e.clientX - rect.left;
    const mouseY = e.clientY - rect.top;

    const filteredRooms = rooms.filter(r => coordinates[r.number] && coordinates[r.number].z === currentZ);
    if (filteredRooms.length === 0) return;

    let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity;
    filteredRooms.forEach(r => {
      const coord = coordinates[r.number];
      if (coord.x < minX) minX = coord.x;
      if (coord.y < minY) minY = coord.y;
      if (coord.x > maxX) maxX = coord.x;
      if (coord.y > maxY) maxY = coord.y;
    });
    const offsetX = (canvasRef.current.width / 2) - ((minX + maxX) / 2 * zoom);
    const offsetY = (canvasRef.current.height / 2) - ((minY + maxY) / 2 * zoom);

    const found = filteredRooms.find(room => {
      const coord = coordinates[room.number];
      const posX = coord.x * zoom + offsetX;
      const posY = coord.y * zoom + offsetY;
      const dist = Math.sqrt((mouseX - posX)**2 + (mouseY - posY)**2);
      return dist < 15;
    });

    if (found) {
      onRoomSelect(found);
    }
  };

  return (
    <canvas
      ref={canvasRef}
      width={800}
      height={600}
      onClick={handleClick}
      style={{ cursor: 'pointer', border: '1px solid #000' }}
    />
  );
};

export default MapCanvas;
