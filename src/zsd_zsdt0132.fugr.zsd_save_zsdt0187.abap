FUNCTION zsd_save_zsdt0187.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_COMMIT) TYPE  FLAG DEFAULT SPACE
*"  TABLES
*"      IT_007 STRUCTURE  ZSDS007
*"----------------------------------------------------------------------

  CHECK it_007[] IS NOT INITIAL.

  SELECT * FROM zsdt0066
    INTO TABLE @DATA(lt_0066)
          FOR ALL ENTRIES IN @it_007
        WHERE nro_sol_ov = @it_007-nro_sol_ov.

  CHECK sy-subrc EQ 0.

  SELECT * FROM zsdt0187
    INTO TABLE @DATA(lt_0187)
      FOR ALL ENTRIES IN @it_007
        WHERE nro_sol_ov = @it_007-nro_sol_ov.

  LOOP AT lt_0187 ASSIGNING FIELD-SYMBOL(<fs_line>) WHERE vbeln IS INITIAL.

    READ TABLE lt_0066 ASSIGNING FIELD-SYMBOL(<fs_0066>)
      WITH KEY nro_sol_ov = <fs_line>-nro_sol_ov.

    CHECK sy-subrc EQ 0.

    CHECK <fs_0066>-vbeln IS NOT INITIAL.

    <fs_line>-vbeln = <fs_0066>-vbeln.
    <fs_line>-posnr = '00010'.

  ENDLOOP.

  MODIFY zsdt0187 FROM TABLE lt_0187.

  CHECK i_commit = 'X'.

  COMMIT WORK.


ENDFUNCTION.
