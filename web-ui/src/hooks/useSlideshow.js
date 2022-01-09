/* eslint-disable no-shadow */
/* eslint-disable no-restricted-syntax */
/* eslint-disable no-await-in-loop */
import { useState, useCallback, useEffect } from 'react';
import { fetchPhotos, fetchRovers } from '../api';
import useOnKeyDown from './useOnKeyDown';


function throttle(func, timeout) {
  let timer;
  return (...args) => {
    if (timer) return;
    func.apply(this, args);
    timer = setTimeout(() => {
      timer = undefined;
    }, timeout);
  };
}

const loadImage = (src) => new Promise((resolve) => {
  const image = document.createElement('img');
  image.addEventListener('load', () => resolve(image));
  image.src = src;
});

export default () => {
  // collections
  const [rovers, setRovers] = useState([]);
  const [photos, setPhotos] = useState([]);

  // selections
  const [rover, setRover] = useState();
  const [camera, setCamera] = useState();
  const [photoIndex, setPhotoIndex] = useState(0);

  // derived state
  const prevDisabled = photoIndex <= 0;
  const nextDisabled = photoIndex >= photos.length - 1;
  const photo = photos[photoIndex];

  // callbacks
  const selectRover = useCallback((rover) => {
    setRover(rover);
    setCamera(rover.cameras[0]);
  }, [setRover]);


  const nextPhoto = useCallback(
    throttle(
      () => setPhotoIndex((i) => Math.min(i + 1, photos.length - 1)),
      200,
    ),
    [setPhotoIndex, photos.length],
  );

  const prevPhoto = useCallback(
    throttle(
      () => setPhotoIndex((i) => Math.max(i - 1, 0)),
      200,
    ),
    [setPhotoIndex],
  );

  // bind arrow keys to forward and back buttons
  useOnKeyDown(({ key }) => {
    if (key === 'ArrowRight') {
      nextPhoto();
    } else if (key === 'ArrowLeft') {
      prevPhoto();
    }
  }, []);

  // fetch rovers on hook init
  useEffect(() => {
    fetchRovers().then((rovers) => {
      setRovers(rovers);
      selectRover(rovers[0]);
    });
  }, []);

  // fetch photos whenever camera changes
  useEffect(() => {
    if (camera === undefined) return;
    const args = { rover: rover.id, camera: camera.id };
    fetchPhotos(args)
      .then((photos) => {
        setPhotoIndex(0);
        setPhotos(photos);
      });
  }, [camera]);

  // pre-emptively load photos after fetching to cache them
  useEffect(() => {
    (async () => {
      for (const photo of photos) {
        await loadImage(photo.imgSrc);
      }
    })();
  },
  [photos]);

  return {
    rover,
    rovers,
    setRover: selectRover,
    setCamera,
    photo,
    navigation: {
      nextPhoto,
      prevPhoto,
      prevDisabled,
      nextDisabled,
    },
  };
};
