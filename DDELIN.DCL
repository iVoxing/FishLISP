ddelin : dialog {
  label="Elevation block option";
  : row {
    : column {
      : boxed_column {
        label="Base point";
        fixed_height=true;
        : button {
          fixed_width=true;
          label="&Pick point <";
          key="pick_btn";
        }
        : edit_box {
          label="Base point &Y:";
          key="y_box";
        }
      }
      : boxed_row {
        label="Symbol style";
        : radio_column {
          fixed_height=true;
          fixed_width=true;
          width=17;
          alignment=top;
          : radio_button {
            label="Upper left";
            key="ul_rad";
          }
          : radio_button {
            label="Upper right";
            key="ur_rad";
          }
          : radio_button {
            label="Lower left";
            key="dl_rad";
          }
          : radio_button {
            label="Lower right";
            key="dr_rad";
          }
        }
        : image {
          fixed_width=true;
          fixed_height=true;
          width=17;
          height=7;
          key="sld_img";
          color=9;
        }
      }
      : edit_box {
        label="Dimscale:";
        fixed_width=true;
        key="dim_box";
      }
    }
    : column {
      fixed_height=true;
      fixed_width=true;
      alignment=top;
      spacer;
      : button {
        label="Accept";
        key="btn_ok";
        is_default=true;
      }
      cancel_button;
    }
  }
}
