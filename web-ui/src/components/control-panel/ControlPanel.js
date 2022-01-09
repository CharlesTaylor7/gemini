/* eslint-disable react/jsx-props-no-spreading */
/* eslint-disable no-shadow */
import React from 'react';
import './ControlPanel.css';
import Dropdown from './Dropdown';
import Navigation from '../navigation/Navigation';


function ControlPanel(props) {
  const {
    rovers, rover,
    setRover, setCamera,
    navigation,
  } = props;

  const roverOptions = Object.fromEntries(rovers.map((rover) => [rover.id, rover]));

  return (
    <div className="control-panel">
      <Dropdown label="Rover" onSelect={setRover} options={roverOptions} />
      { rover
        ? (
          <Dropdown
            label="Camera"
            onSelect={setCamera}
            options={Object.fromEntries(rover.cameras.map((camera) => [camera.id, camera]))}
          />
        ) : null}
      <Navigation {...navigation} />
    </div>
  );
}

export default ControlPanel;
