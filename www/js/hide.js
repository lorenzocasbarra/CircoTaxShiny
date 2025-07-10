function hideOnClickOutside(ids) {
  const elements = ids.map(id => document.getElementById(id)).filter(Boolean);
  document.addEventListener('click', function (event) {
    const isClickInside = elements.some(el => el.contains(event.target));

    if (!isClickInside) {
      elements.forEach(el => {
        el.style.display = 'none';
      });
    }
  });
}
