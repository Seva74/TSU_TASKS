function createModal(header, text)
{
	let parent = document.getElementById('main');
	let child = document.createElement('div');
	child.className = 'modal';

	child.onclick = function() { removeModal('modal') };

	let modal = document.createElement('div');
	modal.className = 'modal-inner';

	child.appendChild(modal);
	modal.innerHTML =  (`<h2>${header}</h2> <p>${text}</p>`);

	parent.appendChild(child);
}

function removeModal(id)
{
	let modal = document.getElementsByClassName('modal')[0];
	modal.remove();
}