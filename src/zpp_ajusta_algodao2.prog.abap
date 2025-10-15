*&---------------------------------------------------------------------*
*& Report ZPP_AJUSTA_ALGODAO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zpp_ajusta_algodao2.

TYPE-POOLS: truxs.

TABLES: zppt0002.

*——————————————————————–*
* Declaração de variáveis                                            *
*——————————————————————–*
DATA: it_raw TYPE truxs_t_text_data.

DATA: t_final     TYPE TABLE OF zppt0002,
      w_final     TYPE zppt0002,
      t_0002      TYPE TABLE OF zppt0002,
      w_0002      TYPE zppt0002,
      w_0002_del  TYPE zppt0002,
      l_id_cotton TYPE zppt0002-id_cotton.

*——————————————————————–*
*** Parâmetros de seleção
*——————————————————————–*
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS: s_idcoto FOR zppt0002-id_cotton OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b2.


START-OF-SELECTION.
  PERFORM f_selecao.
  PERFORM f_elimina_tab.

FORM f_selecao.

  SELECT *
    FROM zppt0002
    INTO TABLE t_final
   WHERE id_cotton IN s_idcoto.

ENDFORM.

FORM f_elimina_tab.

  SORT t_final BY id_cotton.
  DELETE ADJACENT DUPLICATES FROM t_final COMPARING id_cotton.

  LOOP AT t_final INTO w_final.

    SELECT *
      FROM zppt0002
      INTO TABLE t_0002
     WHERE id_cotton = w_final-id_cotton.

    LOOP AT t_0002 INTO w_0002.
      MODIFY zppt0002_del     FROM w_0002.
      DELETE zppt0002         FROM w_0002.
    ENDLOOP.

    COMMIT WORK.

  ENDLOOP.

ENDFORM.
