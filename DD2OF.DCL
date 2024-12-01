DD2OF : dialog {
    label = "DD2OF Options";
    initial_focus = "accept";
    : row {
      : boxed_column {
        label="Layer setting";
        : toggle {
          label = "Change layer";
          mnemonic= "C";
          key = "chlay";
        }
        : popup_list {
          label = "Layer name: ";
          mnemonic= "L";
          key = "layerlist";
          width=27;
        }
      }
      : boxed_column {
        : row {
          : toggle {
            label = "Delete old objects";
            mnemonic= "D";
            key = "delold";
          }
        }
        : row {
          : column {
            : edit_box {
              label = "Offset distance: ";
              mnemonic = "O";
              key = "ofdist";
            }
          }
          : column {
	    : button {
  	      label = "<";
              mnemonic = "<";
	      key = "pickdis";
	    }
	  }
	}
      }
    }
    : row {
      alignment=centered;
      fixed_width=true;
      ok_cancel;
    }
}
