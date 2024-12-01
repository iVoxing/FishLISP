ddf : dialog {
    label = "Edit text";
    initial_focus = "dim_edit";
    : edit_box {
        label = "Text:";
        key = "dim_edit";
        edit_width = 40;
        edit_limit = 256;
        allow_accept = true;
    }
    ok_cancel;
}
