document.addEventListener('DOMContentLoaded', () => {
    const list = document.getElementById('list');
    const siteInput = document.getElementById('site');
    const loginInput = document.getElementById('login');
    const passwordInput = document.getElementById('password');
    const generateBtn = document.getElementById('generate');
    const saveBtn = document.getElementById('save');
    const countSpan = document.getElementById('count');

    let passwords = JSON.parse(localStorage.getItem('passwords') || '[]');
    renderList();

    generateBtn.addEventListener('click', () => {
        const length = 16;
        const charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()_+-=[]{}|;:,.<>?";
        let password = "";
        for (let i = 0; i < length; i++) {
            password += charset.charAt(Math.floor(Math.random() * charset.length));
        }
        passwordInput.value = password;
    });

    saveBtn.addEventListener('click', () => {
        const site = siteInput.value.trim();
        const login = loginInput.value.trim();
        const password = passwordInput.value.trim();

        if (!site || !login || !password) {
            alert('Заполните все поля!');
            return;
        }

        passwords.push({ site, login, password });
        localStorage.setItem('passwords', JSON.stringify(passwords));

        siteInput.value = '';
        loginInput.value = '';
        passwordInput.value = '';

        renderList();
    });

    function renderList() {
        list.innerHTML = '';
        passwords.forEach((item, index) => {
            const li = document.createElement('li');
            li.innerHTML = `
                <strong>${item.site}</strong><br>
                Логин: ${item.login}<br>
                Пароль: <code>${item.password}</code>
                <button onclick="removePassword(${index})" style="float:right; background:#d93025;">Удалить</button>
            `;
            list.appendChild(li);
        });
        countSpan.textContent = passwords.length;
    }

    window.removePassword = function(index) {
        passwords.splice(index, 1);
        localStorage.setItem('passwords', JSON.stringify(passwords));
        renderList();
    };
});