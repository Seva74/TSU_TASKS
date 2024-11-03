const itemsContainer = document.querySelector('.objects__items');
const savedContainer = document.querySelector('.objects__saved');
const addBtn = document.querySelector('.objects__add');
const saveBtn = document.querySelector('.objects__save');

addBtn.addEventListener('click', () => {
    const newObject = document.createElement('div');

    newObject.classList.add('objects__item');
    newObject.innerHTML = `
        <input type="text" class="objects__input">
        <input type="text" class="objects__input">
        <button type="button" class="objects__btn objects__up-btn">&uarr;</button>
        <button type="button" class="objects__btn objects__down-btn">&darr;</button>
        <button type="button" class="objects__btn objects__close-btn">&times;</button>
    `;
    newObject.querySelector('.objects__up-btn').addEventListener('click', moveObjectUp);
    newObject.querySelector('.objects__down-btn').addEventListener('click', moveObjectDown);
    newObject.querySelector('.objects__close-btn').addEventListener('click', removeObject);

    itemsContainer.append(newObject);
});

saveBtn.addEventListener('click', () => {
    const objectItems = document.querySelectorAll('.objects__item');
    let savedObjects = '{';

    objectItems.forEach((item) => {
        const firstInputValue = item.querySelector('input:nth-child(1)').value;
        const secondInputValue = item.querySelector('input:nth-child(2)').value;

        savedObjects += `"${firstInputValue}":"${secondInputValue}",`;
    });

    savedObjects = savedObjects.slice(0, savedObjects.length - 1);
    savedObjects += '}';

    savedContainer.textContent = savedObjects;
});

const moveObjectUp = ($event) => {
    const currentObj = $event.target.closest('.objects__item');
    const prevObj = currentObj.previousElementSibling;
    prevObj?.before(currentObj);
};

const moveObjectDown = ($event) => {
    const currentObj = $event.target.closest('.objects__item');
    const nextObj = currentObj.nextElementSibling;
    nextObj?.after(currentObj);
};

const removeObject = ($event) => {
    $event.target.closest('.objects__item').remove();
};

document.querySelector('.objects__up-btn').addEventListener('click', moveObjectUp);
document.querySelector('.objects__down-btn').addEventListener('click', moveObjectDown);
document.querySelector('.objects__close-btn').addEventListener('click', removeObject);
