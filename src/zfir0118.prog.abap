*&---------------------------------------------------------------------*
*& Report zfir0118
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir0118.


TABLES: zimt0004.

PARAMETERS: p_sba    TYPE zimt0004-n_sba,
            p_emp    TYPE zimt0004-cod_empresa,
            p_filial TYPE zimt0004-cod_filial.

DATA:
  lo_alv  TYPE REF TO cl_salv_table,
  lo_cols TYPE REF TO cl_salv_columns_table,
  lo_col  TYPE REF TO cl_salv_column.

START-OF-SELECTION.

  SELECT *
    FROM zimt0004
    WHERE ( n_sba       = @p_sba OR @p_sba IS INITIAL )
      AND ( cod_empresa = @p_emp OR @p_emp IS INITIAL )
      AND ( cod_filial  = @p_filial  OR @p_filial  IS INITIAL )
          INTO TABLE @DATA(lt_zimt0004).

  IF sy-subrc <> 0.
    MESSAGE 'Nenhum registro encontrado' TYPE 'I'.
    EXIT.
  ENDIF.

  TRY.
      cl_salv_table=>factory(
        IMPORTING r_salv_table = lo_alv
        CHANGING  t_table      = lt_zimt0004 ).

      lo_cols = lo_alv->get_columns( ).

      DATA(lo_functions) = lo_alv->get_functions( ).
      lo_functions->set_all( abap_true ).

      TRY.
          lo_cols->get_column( 'N_SBA' )->set_short_text( |Número SBA| ).
          lo_cols->get_column( 'COD_EMPRESA' )->set_short_text( |Empresa| ).
          lo_cols->get_column( 'COD_FILIAL' )->set_short_text( |Filial| ).
          lo_cols->get_column( 'N_IMOBILIZADO' )->set_short_text( |Imobilizado| ).
          lo_cols->get_column( 'SUBN_IMOBILIZADO' )->set_short_text( |Subnº| ).
          lo_cols->get_column( 'N_EQUIPAMENTO_PM' )->set_short_text( |Equipamento PM| ).
          lo_cols->get_column( 'DATA_BAIXA' )->set_short_text( |Data da Baixa| ).
          lo_cols->get_column( 'MOTIVO_BAIXA' )->set_short_text( |Motivo da Baixa| ).
          lo_cols->get_column( 'EXECUTADO' )->set_short_text( |Executado| ).
        CATCH cx_salv_not_found.

      ENDTRY.

      lo_alv->display( ).

    CATCH cx_salv_msg INTO DATA(lx_msg).
      MESSAGE lx_msg->get_text( ) TYPE 'E'.
  ENDTRY.
