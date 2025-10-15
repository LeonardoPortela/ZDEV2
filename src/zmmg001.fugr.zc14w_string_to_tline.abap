FUNCTION ZC14W_STRING_TO_TLINE.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_STRING) TYPE  C
*"  TABLES
*"      E_TLINE_TAB STRUCTURE  TLINE
*"----------------------------------------------------------------------

  DATA: L_CHAR    TYPE C.             " one character
  DATA: L_TDLINE_STRING_LENGTH TYPE I." actual length to copy to tdline
  DATA: L_STRING_LENGTH TYPE I.
  FIELD-SYMBOLS: <STRING>.
  DATA: L_POS_FIRST TYPE I.
  DATA: L_POS_LAST  TYPE I.
  DATA: L_LAST  TYPE I.
  DATA: L_FDPOS     TYPE I.
  DATA: L_TDFORMAT  LIKE TLINE-TDFORMAT.
  DATA: L_ASCII_LF(2) TYPE C.

  DATA: LC_MAX_TDLINE_LENGTH      TYPE I VALUE 70.
  DATA: LC_MAX_STREAM_LINE_LENGTH TYPE I VALUE 2550.

  CONSTANTS: LC_POSTSCRIPT_CHARS(127) VALUE
     '%0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'.

  DATA: BEGIN OF IC_ASCII,
         CRLF(2)    TYPE X VALUE '0D0A',
         CR(1)      TYPE X VALUE '0D',
         LF(1)      TYPE X VALUE '0A',
         EOF(1)     TYPE X VALUE '04',
         BLANK(1)   TYPE X VALUE '20',
      END OF IC_ASCII.

*   (Rem.: for unicode)

* Function body --------------------------------------------------------

* initialize data
  CLEAR L_TDFORMAT.
  E_TLINE_TAB-TDFORMAT = '/'.
  L_STRING_LENGTH      = STRLEN( I_STRING ).
  L_POS_FIRST          = 0.

* cat the whole string into parts
  WHILE L_POS_FIRST < L_STRING_LENGTH.

* ----> CS1109026 --->
    L_POS_LAST = L_POS_FIRST + LC_MAX_TDLINE_LENGTH.

    if L_POS_LAST GT L_STRING_LENGTH.
       L_POS_LAST = L_POS_FIRST.
       LC_MAX_TDLINE_LENGTH = L_STRING_LENGTH - L_POS_LAST .
    endif.
* <---- CS1109026 <---

*   search for the first lf
    ASSIGN I_STRING+L_POS_FIRST(LC_MAX_TDLINE_LENGTH) TO <STRING>.
*    SEARCH <STRING> FOR IC_ASCII-LF.
    L_ASCII_LF = IC_ASCII-LF.

* ----> CS1109026 --->
    if <STRING> is assigned.
* <---- CS1109026 <---
        SEARCH <STRING> FOR L_ASCII_LF.
    endif.

*     (Rem.: for unicode)
    IF SY-SUBRC IS INITIAL.
      L_FDPOS = SY-FDPOS + L_POS_FIRST.
    ELSE.
      L_FDPOS = L_STRING_LENGTH + 10.
    ENDIF.

*   check if we have to cut the line at a lf
    IF L_POS_LAST >= L_FDPOS.
      L_TDLINE_STRING_LENGTH = L_FDPOS - L_POS_FIRST.
      IF L_TDLINE_STRING_LENGTH > 0.
        E_TLINE_TAB-TDLINE =
                         I_STRING+L_POS_FIRST(L_TDLINE_STRING_LENGTH).
      ENDIF.
      L_POS_LAST = L_FDPOS + 1.     " skip the lf
      L_TDFORMAT = '/'.

*   check if we have to cut the line at a blank
    ELSEIF L_POS_LAST < L_STRING_LENGTH.

*     search for the blank to cut the line
      L_CHAR = I_STRING+L_POS_LAST(1).
      WHILE L_POS_LAST >  L_POS_FIRST AND
            L_CHAR     <> ' '.
        L_POS_LAST = L_POS_LAST - 1.
        L_CHAR = I_STRING+L_POS_LAST(1).
      ENDWHILE.                     " l_pos_last > l_pos_first

*     if the blank wasn't found take the whole line length
      IF L_POS_LAST = L_POS_FIRST.
        L_POS_LAST            = L_POS_FIRST + LC_MAX_TDLINE_LENGTH.
        E_TLINE_TAB-TDLINE = I_STRING+L_POS_FIRST(LC_MAX_TDLINE_LENGTH).
        L_TDFORMAT = '='.
      ELSE.
        L_TDLINE_STRING_LENGTH = L_POS_LAST - L_POS_FIRST.
        E_TLINE_TAB-TDLINE =
                         I_STRING+L_POS_FIRST(L_TDLINE_STRING_LENGTH).
        L_POS_LAST         = L_POS_LAST + 1.     " skip the blank
      ENDIF.

    ELSE.

*     we have to copy the whole string
      E_TLINE_TAB-TDLINE = I_STRING+L_POS_FIRST.
      CLEAR L_TDFORMAT.

    ENDIF.                        " l_pos_last > l_string_length

*   append the part of the string to the table
    APPEND E_TLINE_TAB.
    CLEAR E_TLINE_TAB.
    E_TLINE_TAB-TDFORMAT = L_TDFORMAT.
    CLEAR L_TDFORMAT.

*   search for the next string
    L_POS_FIRST = L_POS_LAST.

  ENDWHILE.                       " l_length < l_string_length




ENDFUNCTION.
