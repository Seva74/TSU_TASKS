const signs = ['+', '-', '*', '/'];
const clearSign = 'C';
const removeOneSymbolSign = '<-';
const equalSign = '=';
const dotSign = '.';

const btnsContainer = document.querySelector('.calculator__btns');
const input = document.querySelector('.calculator__input input');

btnsContainer.addEventListener('click', ($event) => {
    const el = $event.target;
    const inputVal = input.value;

    if (el.classList.contains('calculator__btn')) {
        const newSymbol = el.textContent;

        hasUnallowedSymbols(inputVal) ? input.value = '' : calc(inputVal, newSymbol);
    }
});

const hasUnallowedSymbols = (inputVal) => {
   return !((inputVal).match(/[0-9%\/*\-+=]/) || inputVal === '');
};

const calc = (inputVal, newSymbol) => {
    if (newSymbol === equalSign) {
        clickEqual(inputVal);
    }
    else if (newSymbol === removeOneSymbolSign) {
        input.value = inputVal.substring(0, inputVal.length - 1);
    }
    else if (newSymbol === clearSign) {
        input.value = '';
    }
    else if (newSymbol === dotSign) {
        clickDot(inputVal);
    }
    else {
        signs.includes(newSymbol) ? clickSign(inputVal, newSymbol) : input.value += newSymbol;
    }
}

const clickEqual = (inputVal) => {
    if (isLastSymbolSign(inputVal) || isLastSymbolDot(inputVal)) {
        input.value = inputVal.substring(0, inputVal.length - 1);
    }

    input.value = eval(input.value);
};

const clickSign = (inputVal, newSymbol) => {
    if (isFirstSymbolSign(inputVal) && newSymbol !== '-') {
        input.value = input.value;
    } else if (isLastSymbolSign(inputVal) || isLastSymbolDot(inputVal)) {
        input.value = inputVal.substring(0, inputVal.length - 1) + newSymbol;
    }
    else {
        input.value += newSymbol;
    }
};

const clickDot = (inputVal) => {
    if (isLastSymbolSign(inputVal) || isLastSymbolDot(inputVal)) {
        return;
    }

    let dotsCount = 0;

    for (let i = inputVal.length - 1; i >= 0; i--) {
        if (inputVal[i] === dotSign) {
            dotsCount++;
        }

        if (signs.includes(inputVal[i])) {
            break;
        }
    }

    if (dotsCount !== 0) {
        input.value = input.value;
    } else {
        input.value += dotSign;
    }
};

const isFirstSymbolSign = (inputVal) => {
    return inputVal.length === 0;
};

const isLastSymbolSign = (inputVal) => {
    const lastSymbol = inputVal.slice(inputVal.length - 1, inputVal.length);
    return signs.includes(lastSymbol);
};

const isLastSymbolDot = (inputVal) => {
    const lastSymbol = inputVal.slice(inputVal.length - 1, inputVal.length);
    return '.' === lastSymbol;
};
