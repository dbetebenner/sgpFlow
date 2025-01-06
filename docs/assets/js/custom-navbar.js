document.addEventListener('DOMContentLoaded', function() {
  // Select the navbar-brand <a> tag that contains both the logo and the title
  const navbarBrand = document.querySelector('.navbar-brand');
  
  // Find the logo <img> inside the navbar-brand
  const logo = navbarBrand.querySelector('img');
  
  // Create a new <a> tag for the package title
  const titleElement = navbarBrand.querySelector('.navbar-title');
  const titleLink = document.createElement('a');
  
  // Set the separate href for the package title
  titleLink.href = 'https://centerforassessment.github.io/packageSkeleton/';
  titleLink.classList.add('navbar-brand-title');
  // titleLink.textContent = titleElement.textContent; // Preserve the title text
  
  // Insert the new <a> tag for the title next to the logo
  navbarBrand.appendChild(titleLink);
  
  // Modify the href of the original <a> tag to only apply to the logo
  navbarBrand.href = 'https://centerforassessment.github.io/';
  
  // Remove the original title span, since we now have a new <a> tag for it
  titleElement.remove();
});
