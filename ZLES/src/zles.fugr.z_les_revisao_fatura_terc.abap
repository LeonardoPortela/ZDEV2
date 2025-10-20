FUNCTION z_les_revisao_fatura_terc.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(P_ZLEST0034) TYPE  ZLEST0034 OPTIONAL
*"----------------------------------------------------------------------


  IF p_zlest0034 IS INITIAL.

    SELECT * INTO p_zlest0034
      FROM zlest0034
     WHERE re_belnr NE space.

      UPDATE zlest0032
         SET belnr  = p_zlest0034-re_belnr
             gjahr  = p_zlest0034-re_gjahr
             docnum = p_zlest0034-en_docnum
       WHERE tknum EQ p_zlest0034-tknum.
    ENDSELECT.

    COMMIT WORK.

  ELSE.

    UPDATE zlest0032
       SET belnr  = p_zlest0034-re_belnr
           gjahr  = p_zlest0034-re_gjahr
           docnum = p_zlest0034-en_docnum
     WHERE tknum EQ p_zlest0034-tknum.

    COMMIT WORK.

  ENDIF.

ENDFUNCTION.
