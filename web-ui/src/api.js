export const fetchRovers = () => fetch('/rovers').then((res) => res.json());

export const fetchPhotos = ({ rover, camera }) => fetch(
  `/photos?rover=${rover}&camera=${camera}`,
).then((res) => res.json());
