f_stair : dialog {
  label="Stair option";
  : boxed_column {
    label="Setting";
    : row {
      : boxed_column {
        label="Segment";
        alignment=top;
        : edit_box {
          label="&Height:";
          key="st_h_box";
        }
        : edit_box {
          label="&Width: ";
          key="st_w_box";
        }
        : text {
          label="Length:";
          key="len_txt";
        }
      }
      : boxed_column {
        label="Steps";
        alignment=top;
        : edit_box {
          label="He&ight:";
          key="sp_h_box";
        }
        : edit_box {
          label="Wi&dth: ";
          key="sp_w_box";
        }
        : text {
          label="Amout:";
          key="amt_txt";
        }
      }
    }
    spacer;
    : row {
      alignment=left;
      fixed_width=true;
      : toggle {
        label="&Keep step width";
        key="keep_tgl";
        width=21;
      }
      : button {
        label="&Calculate";
        key="cal_btn";
      }
    }
  }
  : row {
    : column {
      : row {
        : boxed_radio_column {
          label="Direction";
          fixed_height=true;
          fixed_width=true;
          width=13;
          alignment=top;
          : radio_button {
            label="&Left";
            key="left_rad";
          }
          : radio_button {
            label="&Right";
            key="right_rad";
          }
        }
        : boxed_radio_column {
          label="Style";
          fixed_height=true;
          fixed_width=true;
          width=13;
          alignment=top;
          : radio_button {
            label="&Section";
            key="sec_rad";
          }
          : radio_button {
            label="&Elevation";
            key="ele_rad";
          }
        }
      }
      spacer;
      : row {
        alignment=right;
        fixed_height=true;
        fixed_width=true;
        : image {
          key="logo";
          height=3;
          width=20;
          color=9;
        }
        : column {
          spacer;
          ok_button;
        }
      }
    }
    : column {
      : boxed_radio_column {
        label="Position";
        fixed_height=true;
        fixed_width=true;
        width=13;
        alignment=top; 
        : radio_button {
          label="&Top";
          key="top_rad";
        }
        : radio_button {
          label="&Normal";
          key="nor_rad";
          value=1;
        }
        : radio_button {
          label="&Ground";
          key="but_rad";
        }
      }
      spacer; 
      cancel_button;
    }
  }
}