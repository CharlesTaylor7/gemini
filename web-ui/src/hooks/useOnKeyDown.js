import { useEffect } from 'react';

export default (eventHandler, deps) => {
  useEffect(() => {
    document.onkeydown = eventHandler;
    return () => document.removeEventListener('onkeydown', eventHandler);
  }, [deps]);
};
