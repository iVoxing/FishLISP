fish_replace : dialog {
  label="Replace";
  initial_focus="text_find";
  spacer;
  : row {
    : column {
      : edit_box {
        label="Find:      ";
        mnemonic="n";
        key="text_find";
        fixed_width=true;
        width=30;
      }
      : edit_box {
        label="Repalce:";
        mnemonic="p";
        key="text_replace";
        fixed_width=true;
        width=30;
      }
      spacer;
      spacer;
      : toggle {
        label="Match whole word only";
        mnemonic="w";
        key="tgl_match_word";
      }
      : toggle {
        label="Match case";
        mnemonic="c";
        key="tgl_match_case";
      }
    }
    spacer;
    : column {
      fixed_width=true;
      : button {
        label="Find next";
        mnemonic="F";
        key="btn_find";
        width=13;
      }
      : button {
        label="Replace";
        mnemonic="R";
        key="btn_rep_c";
        width=13;
      }
      : button {
        label="Replace All";
        mnemonic="A";
        key="btn_rep_a";
        width=13;
      }
      : button {
        label="Close";
        is_cancel=true;
        width=13;
      }

    }
  }
  errtile;
}