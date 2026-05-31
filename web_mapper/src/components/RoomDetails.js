import React from 'react';
import ColoredText from './ColoredText';

const RoomDetails = ({ room }) => {
  if (!room) return <div className="room-details">Select a room to see details</div>;

  return (
    <div className="room-details" style={{ padding: '20px', borderLeft: '1px solid #ccc', height: '100%', overflowY: 'auto', backgroundColor: '#222', color: '#eee' }}>
      <h2><ColoredText text={room.name} /></h2>
      <p style={{ color: '#aaa' }}>VNUM: {room.number}</p>
      <div style={{ whiteSpace: 'pre-wrap', marginBottom: '20px' }}>
        <ColoredText text={room.description} />
      </div>
      {room.smell && <p><strong>Smell:</strong> <ColoredText text={room.smell} /></p>}
      {room.listen && <p><strong>Listen:</strong> <ColoredText text={room.listen} /></p>}
      <p><strong>Sector:</strong> {room.sector}</p>
      <p><strong>Exits:</strong></p>
      <ul>
        {room.exits.map((exit, index) => (
          <li key={index}>
            <strong>{exit.direction}:</strong> {exit.destination === -1 ? 'None' : exit.destination}
            {exit.description && <div style={{ fontSize: '0.9em', color: '#888' }}><ColoredText text={exit.description} /></div>}
          </li>
        ))}
      </ul>
    </div>
  );
};

export default RoomDetails;
