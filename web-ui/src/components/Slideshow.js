import React from 'react';
import './Slideshow.css';
import ControlPanel from './control-panel/ControlPanel';
import useSlideshow from '../hooks/useSlideshow';

function Slideshow() {
  const {
    rovers, rover,
    setRover, setCamera,
    navigation, photo,
  } = useSlideshow();

  return (
    <div className="slideshow">
      <ControlPanel
        rovers={rovers}
        rover={rover}
        navigation={navigation}
        setRover={setRover}
        setCamera={setCamera}
      />
      { photo
        && (
          <img className="rover-photo" src={photo.imgSrc} />
        )}
    </div>
  );
}

export default Slideshow;
