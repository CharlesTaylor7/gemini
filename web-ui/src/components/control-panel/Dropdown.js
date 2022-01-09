import React from 'react';
import './Dropdown.css';


const Dropdown = ({ onSelect, options, label }) => (
  <div className="dropdown">
    <div className="dropdown-label">
      {label}
      :
    </div>
    <select
      className="dropdown-select"
      onChange={(event) => onSelect(options[event.target.value])}
    >
      {Object.entries(options).map(([id, option]) => (
        <option key={id} value={id}>{option.name}</option>
      ))}
    </select>
  </div>
);

export default Dropdown;
