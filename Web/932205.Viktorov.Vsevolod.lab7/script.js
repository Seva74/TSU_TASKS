function removeElement(elementId) {
    let el = document.getElementById(elementId);
    el.remove();
}

function changeColor(elementId) {
    let el = document.getElementById(elementId);
    el.style.backgroundColor = 'yellow';
}

function createRect(inputId) {
    let el = document.getElementById(inputId);
    let amount = el.value;
    
    for (let i = 0; i < amount; i++) {
        let rect = document.createElement('div');
        //внешний вид
        rect.style.backgroundColor = 'red';
        rect.style.opacity = '0.9';
        rect.style.border = '3px solid black';
        let size = Math.random() * 500; //размер от 0 до 499
        rect.style.height = size;
        rect.style.width = size;
        //положение квадрата
        rect.style.position = 'absolute';
        rect.style.left = Math.random() * (window.innerWidth - size);
        rect.style.top = Math.random() * (window.innerHeight - size);

        let rectId = `rect-${Date.now() + Math.floor(Math.random() * 10000)}`;
        rect.id = rectId;

        rect.ondblclick = function () { removeElement(rectId); };
        rect.onclick = function () { changeColor(rectId); };

        let parent = document.getElementById('figures');
        parent.appendChild(rect);
    }
}

function createTriangle(inputId) {
    let el = document.getElementById(inputId);
    let amount = el.value;
    
    for (let i = 0; i < amount; i++) {
        let triangle = document.createElement('div');
        //внешний вид
        triangle.style.opacity = '0.9';
        let size = Math.random() * 500;
        triangle.style.border = `${size}px solid transparent`;
        triangle.style.borderBottom = `${size}px solid blue`
        triangle.style.boxSizing = 'border-box';
        //положение треугольника
        triangle.style.position = 'absolute';
        triangle.style.left = Math.random() * (window.innerWidth - size * 2);
        triangle.style.top = Math.random() * (window.innerHeight - size * 2);

        let triangleId = `triangle-${Date.now() + Math.floor(Math.random() * 10000)}`;
        triangle.id = triangleId;

        triangle.ondblclick = function () { removeElement(triangleId); };
        triangle.onclick = function () {
            let el = document.getElementById(triangleId);
            el.style.borderBottomColor = 'yellow';
        }

        let parent = document.getElementById('figures');
        parent.appendChild(triangle);
    }
}

function createCircle(inputId) {
    let el = document.getElementById(inputId);
    let amount = el.value;

    for (let i = 0; i < amount; i++) {
        let circle = document.createElement('div');
        //внешний вид
        circle.style.backgroundColor = 'green';
        circle.style.opacity = '0.9';
        circle.style.border = '3px solid black';
        let size = Math.random() * 500; //размер от 0 до 499
        circle.style.height = size;
        circle.style.width = size;
        circle.style.borderRadius = '100%';
        //положение круга
        circle.style.position = 'absolute';
        circle.style.left = Math.random() * (window.innerWidth - size);
        circle.style.top = Math.random() * (window.innerHeight - size);

        let circleId = `circle-${Date.now() + Math.floor(Math.random() * 10000)}`; 
        circle.id = circleId;

        circle.ondblclick = function () { removeElement(circleId); };
        circle.onclick = function () { changeColor(circleId); };
        
        let parent = document.getElementById('figures');
        parent.appendChild(circle);
    }
}