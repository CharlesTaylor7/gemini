import React from 'react';
import './ArrowButton.css';

const ArrowButton = ({ direction, handleClick, disabled }) => (
  <button className="arrow-button" onClick={handleClick} type="button" disabled={disabled}>
    { direction === 'left' ? '<' : '>'}
  </button>
);

export default ArrowButton;
