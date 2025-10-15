FUNCTION-POOL zgfs_int_ob_crt.              "MESSAGE-ID ..

DATA: lv_erro     TYPE c,
      lv_metodo   TYPE string,
      ls_material TYPE zde_int_ob_product,
      ls_parceiro TYPE zde_int_ob_contact,
      ls_tracking TYPE zde_int_ob_tracking,
      ls_retorno  TYPE zintegracao,
*
      t_zsdt0132  TYPE TABLE OF zsdt0132,
      w_zsdt0132  TYPE zsdt0132.

*************************************************************************************
*************************************************************************************
