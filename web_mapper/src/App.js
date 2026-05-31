import React, { useState, useEffect } from 'react';
import MapCanvas from './components/MapCanvas';
import RoomDetails from './components/RoomDetails';
import { calculateCoordinates } from './utils/coordinateCalculator';

function App() {
  const [zones, setZones] = useState([]);
  const [currentZoneId, setCurrentZoneId] = useState(null);
  const [rooms, setRooms] = useState([]);
  const [coordinates, setCoordinates] = useState({});
  const [currentZ, setCurrentZ] = useState(0);
  const [selectedRoom, setSelectedRoom] = useState(null);
  const [availableZs, setAvailableZs] = useState([0]);

  useEffect(() => {
    fetch('/data/zones.json')
      .then(res => res.json())
      .then(data => {
        setZones(data.zones);
        if (data.zones.length > 0) {
          const firstAvailable = data.zones.find(z => z.status === 'available');
          if (firstAvailable) setCurrentZoneId(firstAvailable.id);
        }
      })
      .catch(err => console.error("Failed to load zones.json", err));
  }, []);

  useEffect(() => {
    if (currentZoneId) {
      fetch(`/data/${currentZoneId}.json`)
        .then(res => res.json())
        .then(data => {
          setRooms(data.rooms);
          const coords = calculateCoordinates(data.rooms);
          setCoordinates(coords);

          const zs = [...new Set(Object.values(coords).map(c => c.z))].sort((a, b) => a - b);
          setAvailableZs(zs);
          setCurrentZ(zs.includes(0) ? 0 : (zs.length > 0 ? zs[0] : 0));
          setSelectedRoom(null);
        })
        .catch(err => {
            console.error(`Failed to load zone ${currentZoneId}.json`, err);
            setRooms([]);
            setCoordinates({});
        });
    }
  }, [currentZoneId]);

  return (
    <div className="App" style={{ display: 'flex', flexDirection: 'column', height: '100vh', backgroundColor: '#333', color: '#fff' }}>
      <header style={{ padding: '10px', borderBottom: '1px solid #555', display: 'flex', alignItems: 'center', gap: '20px' }}>
        <h1 style={{ margin: 0, fontSize: '1.5em' }}>CircleMUD Web Mapper</h1>
        <div style={{ display: 'flex', alignItems: 'center', gap: '10px' }}>
          <label>Zone:</label>
          <select value={currentZoneId || ''} onChange={(e) => setCurrentZoneId(e.target.value)}>
            {zones.map(zone => (
              <option key={zone.id} value={zone.id} disabled={zone.status !== 'available'}>
                {zone.name} ({zone.id}) {zone.status !== 'available' ? '[Closed]' : ''}
              </option>
            ))}
          </select>
        </div>
        <div style={{ display: 'flex', alignItems: 'center', gap: '10px' }}>
          <label>Floor:</label>
          <select value={currentZ} onChange={(e) => setCurrentZ(parseInt(e.target.value))}>
            {availableZs.map(z => (
              <option key={z} value={z}>Level {z}</option>
            ))}
          </select>
        </div>
      </header>
      <div style={{ display: 'flex', flex: 1, overflow: 'hidden' }}>
        <div style={{ flex: 1, display: 'flex', justifyContent: 'center', alignItems: 'center', backgroundColor: '#000', overflow: 'auto' }}>
          <MapCanvas
            rooms={rooms}
            coordinates={coordinates}
            currentZ={currentZ}
            selectedRoom={selectedRoom}
            onRoomSelect={setSelectedRoom}
          />
        </div>
        <div style={{ width: '400px', backgroundColor: '#222' }}>
          <RoomDetails room={selectedRoom} />
        </div>
      </div>
    </div>
  );
}

export default App;
