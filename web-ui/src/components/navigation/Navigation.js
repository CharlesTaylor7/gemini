import React from 'react';
import ArrowButton from './ArrowButton';
import './Navigation.css';

const Navigation = ({ prevPhoto, nextPhoto, prevDisabled, nextDisabled }) => (
  <div className="navigation">
    <ArrowButton direction="left" handleClick={prevPhoto} disabled={prevDisabled} />
    <ArrowButton direction="right" handleClick={nextPhoto} disabled={nextDisabled} />
  </div>
);

export default Navigation;
