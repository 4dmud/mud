export function calculateCoordinates(rooms) {
  if (!rooms || rooms.length === 0) return {};

  const roomMap = new Map(rooms.map(r => [r.number, r]));
  const coordinates = {};
  const visited = new Set();
  const queue = [];

  // Start with the first room in the list
  const startRoom = rooms[0];
  queue.push({ number: startRoom.number, x: 0, y: 0, z: 0 });
  visited.add(startRoom.number);

  while (queue.length > 0) {
    const { number, x, y, z } = queue.shift();
    coordinates[number] = { x, y, z };

    const room = roomMap.get(number);
    if (!room || !room.exits) continue;

    room.exits.forEach(exit => {
      const dest = exit.destination;
      if (dest !== -1 && !visited.has(dest) && roomMap.has(dest)) {
        let nextX = x;
        let nextY = y;
        let nextZ = z;

        switch (exit.direction.toLowerCase()) {
          case 'north': nextY -= 1; break;
          case 'south': nextY += 1; break;
          case 'east': nextX += 1; break;
          case 'west': nextX -= 1; break;
          case 'up': nextZ += 1; break;
          case 'down': nextZ -= 1; break;
          case 'northeast': nextX += 1; nextY -= 1; break;
          case 'northwest': nextX -= 1; nextY -= 1; break;
          case 'southeast': nextX += 1; nextY += 1; break;
          case 'southwest': nextX -= 1; nextY += 1; break;
        }

        visited.add(dest);
        queue.push({ number: dest, x: nextX, y: nextY, z: nextZ });
      }
    });
  }

  // Handle disconnected rooms
  rooms.forEach(room => {
    if (!visited.has(room.number)) {
      const islandQueue = [{ number: room.number, x: 0, y: 100, z: 0 }];
      visited.add(room.number);

      while(islandQueue.length > 0) {
          const { number, x, y, z } = islandQueue.shift();
          coordinates[number] = { x, y, z };
          const r = roomMap.get(number);
          if (!r || !r.exits) continue;
          r.exits.forEach(exit => {
              const dest = exit.destination;
              if (dest !== -1 && !visited.has(dest) && roomMap.has(dest)) {
                  let nextX = x, nextY = y, nextZ = z;
                  switch (exit.direction.toLowerCase()) {
                      case 'north': nextY -= 1; break;
                      case 'south': nextY += 1; break;
                      case 'east': nextX += 1; break;
                      case 'west': nextX -= 1; break;
                      case 'up': nextZ += 1; break;
                      case 'down': nextZ -= 1; break;
                  }
                  visited.add(dest);
                  islandQueue.push({ number: dest, x: nextX, y: nextY, z: nextZ });
              }
          });
      }
    }
  });

  return coordinates;
}
