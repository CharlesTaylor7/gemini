import { fetchPhotos } from './api';

describe('fetchPhotos', () => {
  test('integration test', async () => {
    const [photos1, photos2] = await Promise.all([
      fetchPhotos({
        rover: 'Curiosity',
        camera: 'FHAZ',
        method: 'SQL',
      }),
      fetchPhotos({
        rover: 'Curiosity',
        camera: 'FHAZ',
        method: 'ORM',
      }),
    ]);
    expect(photos1).toMatchSnapshot();
    expect(photos1).toEqual(photos2);
  });
});
