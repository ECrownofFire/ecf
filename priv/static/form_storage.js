// https://stackoverflow.com/questions/42406520/populate-an-html-form-from-a-formdata-object
function formSerialize(form) {
    const data = new FormData(form);
    return new URLSearchParams(data).toString();
}

function formDeserialize(form, data) {
    const entries = (new URLSearchParams(data)).entries();
    let entry;
    while(!(entry = entries.next()).done) {
        const [key, val] = entry.value;
        var input = form.elements[key];

        switch(input.type) {
            case 'checkbox': input.checked = !!val; break;
            default:         input.value = val;     break;
        }
    }
}

function clearFormStorage() {
    sessionStorage.removeItem('restore');
    sessionStorage.removeItem('form');
    sessionStorage.removeItem('loc');
    sessionStorage.removeItem('id');
}

function saveForm(form) {
    sessionStorage.setItem('form', formSerialize(form));
    sessionStorage.setItem('loc', form.dataset.loc);
    sessionStorage.setItem('id', form.dataset.id);
}

(function() {
    var forms = document.getElementsByClassName('form-save');

    for (var i = 0; i < forms.length; i++) {
        var form = forms[i];

        if (sessionStorage.getItem('restore')
            && sessionStorage.getItem('loc') == form.dataset.loc
            && sessionStorage.getItem('id') == form.dataset.id) {
            var data = sessionStorage.getItem('form');
            formDeserialize(form, data);
        }

        clearFormStorage();

        form.addEventListener('click', function() { saveForm(form); }, false);
    }
})();

