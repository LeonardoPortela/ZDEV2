************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Exportação & Importação Ltda                 *
* Data desenv ...: 24.09.2022                                         *
* Tipo de prg ...: executável                                          *
* Objetivo    ...: Atualização de campo                                *
*                                                                      *
************************************************************************
* Data Modif    Autor                Empresa          Ticket           *
************************************************************************
* 24.09.2022    Jose Alberto Medici  Rimini      IR112301/CS1027702    *
*                                                                      *
************************************************************************

REPORT  zmmt0901a.

TABLES: zppt0002, zppt0030.

DATA: BEGIN OF  t_zppt0002 OCCURS 0.
        INCLUDE STRUCTURE zppt0002.
DATA: END   OF t_zppt0002.

DATA: w_zppt0002 LIKE zppt0002,
      w_acharg   LIKE zppt0002-acharg,
      w_index    LIKE sy-index VALUE 1.

DATA: BEGIN OF  t_zppt0030 OCCURS 0.
        INCLUDE STRUCTURE zppt0030.
DATA: END   OF t_zppt0030.

DATA: w_zppt0030 LIKE zppt0030,
      w_cotton   LIKE zppt0030-id_cotton.


SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-008.

  SELECT-OPTIONS:
    v_acharg FOR zppt0002-acharg.
  PARAMETERS: w_varios AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

CLEAR t_zppt0002. REFRESH t_zppt0002.

LOOP AT v_acharg.

  w_acharg = v_acharg-low.

  CLEAR t_zppt0002. REFRESH t_zppt0002.

*  SELECT *
*    INTO TABLE t_zppt0002
*    FROM zppt0002
*    WHERE acharg EQ  w_acharg.

  SELECT SINGLE * INTO w_zppt0002
    FROM zppt0002
    WHERE acharg EQ  w_acharg.

  IF sy-subrc EQ 0.

    w_cotton = w_zppt0002-id_cotton.

    UPDATE zppt0002 SET
       status = 'R'
    WHERE acharg EQ w_acharg.

    IF sy-subrc EQ 0.

      CLEAR t_zppt0030. REFRESH t_zppt0030.

      SELECT *
        INTO TABLE t_zppt0030
        FROM zppt0030
        WHERE id_cotton EQ w_cotton
          AND acharg EQ  w_acharg.

      IF sy-subrc EQ 0.
*-CS1132633-#RIMINI-17.08.2023-BEGIN
* --->  CS1045142 - IR120134
*    SORT t_zppt0030 BY data_proc hora_proc DESCENDING.
* <---  CS1045142 - IR120134
        SORT t_zppt0030 BY data_proc DESCENDING
                           hora_proc DESCENDING.
*-CS1132633-#RIMINI-17.08.2023-END
        READ TABLE t_zppt0030 INDEX 1 INTO w_zppt0030.
        IF sy-subrc EQ 0.

          UPDATE zppt0030 SET
           emproc_normal = ''
           proces_normal = ''
          WHERE id_cotton EQ w_zppt0030-id_cotton
            AND acharg EQ w_zppt0030-acharg
            AND hora_proc EQ w_zppt0030-hora_proc.

        ENDIF.
      ENDIF.
    ENDIF.

    COMMIT WORK.

    IF w_varios NE 'X'.
      IF sy-subrc EQ 0.
        MESSAGE 'REGISTRO ALTERADO' TYPE 'I'.
      ELSE.
        MESSAGE 'REGISTRO NAO ALTERADO' TYPE 'I'.
      ENDIF.
    ENDIF.
  ELSE.
    IF w_varios NE 'X'.
      MESSAGE 'REGISTRO NAO EXISTE' TYPE 'I'.
    ENDIF.
  ENDIF.
  w_index = w_index + 1.

ENDLOOP.
