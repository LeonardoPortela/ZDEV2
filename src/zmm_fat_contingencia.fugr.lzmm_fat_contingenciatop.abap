FUNCTION-POOL zmm_fat_contingencia.         "MESSAGE-ID ..

DATA: l_destination   TYPE char40,
      l_importado     TYPE char1,
      t_zmmt0008_ecc  TYPE TABLE OF zmmt0008,
      w_zmmt0008_ecc  TYPE zmmt0008,
      t_zmmt0008_hana TYPE TABLE OF zmmt0008,
      w_zmmt0008_hana TYPE zmmt0008,
      w_layout        TYPE slis_layout_alv,
      t_fieldcat      TYPE slis_t_fieldcat_alv,
      t_alv           TYPE TABLE OF zmmt0008,
      w_alv           TYPE TABLE OF zmmt0008,
      l_grid_title    TYPE lvc_title,
      l_program       TYPE sy-repid.
