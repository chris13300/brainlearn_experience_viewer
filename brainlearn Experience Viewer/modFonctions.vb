Imports System.Drawing.Imaging
Imports System.IO
Imports VB = Microsoft.VisualBasic

Module modFonctions
    Public processus As System.Diagnostics.Process
    Public entree As System.IO.StreamWriter
    Public sortie As System.IO.StreamReader
    Public moteur_court As String
    Public analyse As Boolean
    Public profMinAnalyse As Integer
    Public profMaxAnalyse As Integer
    Public modeAnalyse As Integer

    Public Function binToMove(chaineBIN As String) As String
        Dim move As String

        '0 111 110 000 111 001
        '  Q   7   a   8   b

        move = ""

        Select Case gauche(droite(chaineBIN, 9), 3) 'a
            Case "000"
                move = move & "a"

            Case "001"
                move = move & "b"

            Case "010"
                move = move & "c"

            Case "011"
                move = move & "d"

            Case "100"
                move = move & "e"

            Case "101"
                move = move & "f"

            Case "110"
                move = move & "g"

            Case "111"
                move = move & "h"

        End Select

        Select Case gauche(droite(chaineBIN, 12), 3) '7
            Case "000"
                move = move & "1"

            Case "001"
                move = move & "2"

            Case "010"
                move = move & "3"

            Case "011"
                move = move & "4"

            Case "100"
                move = move & "5"

            Case "101"
                move = move & "6"

            Case "110"
                move = move & "7"

            Case "111"
                move = move & "8"

        End Select

        Select Case droite(chaineBIN, 3) 'b
            Case "000"
                move = move & "a"

            Case "001"
                move = move & "b"

            Case "010"
                move = move & "c"

            Case "011"
                move = move & "d"

            Case "100"
                move = move & "e"

            Case "101"
                move = move & "f"

            Case "110"
                move = move & "g"

            Case "111"
                move = move & "h"

        End Select

        Select Case gauche(droite(chaineBIN, 6), 3) '8
            Case "000"
                move = move & "1"

            Case "001"
                move = move & "2"

            Case "010"
                move = move & "3"

            Case "011"
                move = move & "4"

            Case "100"
                move = move & "5"

            Case "101"
                move = move & "6"

            Case "110"
                move = move & "7"

            Case "111"
                move = move & "8"

        End Select


        Select Case droite(gauche(chaineBIN, 4), 3) 'Q
            Case "001"
                move = move & "N"

            Case "101"
                move = move & "B"

            Case "110"
                move = move & "R"

            Case "111"
                move = move & "Q"

        End Select

        'grand roque blanc ou noir
        If gauche(chaineBIN, 4) = "1100" Then
            If move = "e1a1" Or move = "e8a8" Then
                move = Replace(move, "a", "c")
            End If
        End If

        Return move
    End Function

    Public Function brainlearn_expListe(fichierBIN As String, keyUCI As String) As String
        Dim tabTampon(23) As Byte, chaineExperience As String, compteur As Integer
        Dim tabBIN(0) As Byte, pos As Long, i As Integer, j As Integer
        Dim tabCoups(1000) As String, tabProfondeurs(1000) As Integer, tabScores(1000) As Integer, tabPerformances(1000) As Integer
        Dim coup As String, profondeur As Integer, scoreCP As String, scoreMAT As String, performance As Integer
        Dim keyBIN As String
        Dim lectureBIN As IO.FileStream, posLecture As Long, tailleBIN As Long, tailleTampon As Long, reservation As Boolean

        If Not My.Computer.FileSystem.FileExists(fichierBIN) Then
            Return ""
        End If

        posLecture = 0
        tailleBIN = FileLen(fichierBIN)
        tailleTampon = tailleBIN
        i = 50
        lectureBIN = New IO.FileStream(fichierBIN, IO.FileMode.Open, IO.FileAccess.Read, IO.FileShare.ReadWrite)

        compteur = 1
        While posLecture < tailleBIN
            If posLecture + tailleTampon <= tailleBIN Then
                reservation = False
                Do
                    Try
                        ReDim tabBIN(tailleTampon - 1)
                        reservation = True
                    Catch ex As Exception
                        i = i - 1
                        tailleTampon = 24 * i * 1000000
                    End Try
                Loop Until reservation
                lectureBIN.Read(tabBIN, 0, tabBIN.Length)
            Else
                tailleTampon = tailleBIN - posLecture
                ReDim tabBIN(tailleTampon - 1)
                lectureBIN.Read(tabBIN, 0, tabBIN.Length)
            End If

            pos = 0
            Do
                Array.Copy(tabBIN, pos, tabTampon, 0, 24) 'clé inversée
                keyBIN = ""
                For i = 0 To 7
                    keyBIN = keyBIN & hexa(tabTampon(i))
                Next

                If keyUCI = keyBIN Then
                    'inversed key (8 octets) | Depth |          | score       | inv. move |       | Performance |
                    'h0 h1 h2 h3 h4 h5 h6 h7 | h8    | h9 hA hB | hC hD hE hF | h0 h1     | h2 h3 | h4          | h5 h6 h7 (24 octets)
                    '00 01 02 03 04 05 06 07   08      09 10 11   12 13 14 15   16 17       18 19   20            21 22 23 tabTampon
                    '-----------------------------------------------------------------------------------------------------
                    'fb 59 2f 56 d4 01 8f 8f | 1e    | 00 00 00 | 3f 00 00 00 | 1c 03     | 00 00 | 64          | 00 00 00
                    'decimal                   30                 0000003f      031c                100%
                    '										      63/208        001 100 011 100
                    '										      +0.30         2   e   4   e
                    '                                                           e2e4

                    coup = hexa(tabTampon(17)) & hexa(tabTampon(16))
                    '031c => 001 100 011 100 => e2e4
                    coup = binToMove(hexadecimalToBinaire(coup))

                    profondeur = tabTampon(8)

                    scoreCP = hexa(tabTampon(15)) & hexa(tabTampon(14)) & hexa(tabTampon(13)) & hexa(tabTampon(12))
                    scoreMAT = scoreCP
                    'score positif
                    If tabTampon(15) = 0 And tabTampon(14) = 0 Then
                        If hexa(tabTampon(13)) = "7D" And hexa(tabTampon(12)) = "02" Then
                            scoreCP = "<empty>"
                        ElseIf hexa(tabTampon(13)) = "7C" Then
                            scoreMAT = "+" & Format((32001 - Convert.ToInt64(scoreMAT, 16)) / 2, "0")
                            scoreCP = ""
                        Else
                            scoreCP = Format(Convert.ToInt64(scoreCP, 16) / 2.08, "0")
                        End If
                    ElseIf tabTampon(15) = 255 And tabTampon(14) = 255 Then
                        If hexa(tabTampon(13)) = "83" Then
                            scoreMAT = "-" & Format((32000 - (4294967296 - Convert.ToInt64(scoreMAT, 16))) / 2, "0")
                            scoreCP = ""
                        Else
                            scoreCP = Format((Convert.ToInt64(scoreCP, 16) - 4294967295) / 2.08, "0")
                        End If
                    Else
                        MsgBox("scoreCP = " & scoreCP, MsgBoxStyle.Exclamation, "en travaux")
                    End If

                    performance = tabTampon(20)

                    tabCoups(compteur) = coup
                    tabProfondeurs(compteur) = profondeur
                    If scoreCP <> "" Then
                        tabScores(compteur) = CInt(Replace(scoreCP, "<empty>", "0"))
                    Else
                        tabScores(compteur) = CInt(scoreMAT) * 1000
                    End If
                    tabPerformances(compteur) = performance
                    '1 : e2e4, depth: 30, eval: cp 30, quality: 100

                    compteur = compteur + 1
                End If

                pos = pos + 24
            Loop While pos < tabBIN.Length

            posLecture = lectureBIN.Position
            tabBIN = Nothing
        End While
        lectureBIN.Close()
        compteur = compteur - 1

        'on classe les scores
        For i = 1 To compteur
            For j = 1 To compteur
                If tabScores(j) < tabScores(i) _
                Or (tabScores(j) = tabScores(i) And tabProfondeurs(j) < tabProfondeurs(i)) Then
                    coup = tabCoups(i)
                    tabCoups(i) = tabCoups(j)
                    tabCoups(j) = coup

                    profondeur = tabProfondeurs(i)
                    tabProfondeurs(i) = tabProfondeurs(j)
                    tabProfondeurs(j) = profondeur

                    pos = tabScores(i)
                    tabScores(i) = tabScores(j)
                    tabScores(j) = pos

                    performance = tabPerformances(i)
                    tabPerformances(i) = tabPerformances(j)
                    tabPerformances(j) = performance
                End If
            Next
        Next

        'on rassemble les infos
        chaineExperience = ""
        For i = 1 To compteur
            chaineExperience = chaineExperience & i & " : " & tabCoups(i) & ", depth: " & tabProfondeurs(i)
            If tabScores(i) <= -1000 Or 1000 <= tabScores(i) Then
                chaineExperience = chaineExperience & ", eval: mate " & Format(tabScores(i) / 1000, "0")
            Else
                chaineExperience = chaineExperience & ", eval: cp " & tabScores(i)
            End If

            chaineExperience = chaineExperience & ", quality: " & tabPerformances(i) & vbCrLf
        Next

        Return chaineExperience

    End Function

    Public Sub chargerMoteur(chemin As String)
        Dim chaine As String
starting:
        Try
            processus = New System.Diagnostics.Process()

            processus.StartInfo.RedirectStandardOutput = True
            processus.StartInfo.UseShellExecute = False
            processus.StartInfo.RedirectStandardInput = True
            processus.StartInfo.CreateNoWindow = True
            processus.StartInfo.WorkingDirectory = My.Application.Info.DirectoryPath
            processus.StartInfo.FileName = chemin
            processus.Start()
            processus.PriorityClass = 64 '64 (idle), 16384 (below normal), 32 (normal), 32768 (above normal), 128 (high), 256 (realtime)

            entree = processus.StandardInput
            sortie = processus.StandardOutput

            entree.WriteLine("uci")
            chaine = ""
            While InStr(chaine, "uciok") = 0
                chaine = sortie.ReadLine
                Threading.Thread.Sleep(1)
            End While

            entree.WriteLine("setoption name threads value 1")

            entree.WriteLine("isready")
            chaine = ""
            While InStr(chaine, "readyok") = 0
                chaine = sortie.ReadLine
                Threading.Thread.Sleep(1)
            End While
        Catch ex As Exception
            If processus.HasExited Then
                entree.Close()
                sortie.Close()
                GoTo starting
            End If
        End Try

    End Sub

    Public Sub dechargerMoteur()
        Try
            entree.Close()
            sortie.Close()
            processus.Close()
        Catch ex As Exception

        End Try

        entree = Nothing
        sortie = Nothing
        processus = Nothing
    End Sub

    Public Function defragBIN(cheminBIN As String, profMin As Integer) As String
        Dim tabBIN(0) As Byte, i As Long, tabNEW() As Byte, offset As Long
        Dim tabTampon(23) As Byte, compteur As Integer, nbSuppression As Integer
        Dim message As String, pos As Long
        Dim lectureBIN As IO.FileStream, posLecture As Long, tailleBIN As Long, tailleTampon As Long, reservation As Boolean

        message = ""

        If My.Computer.FileSystem.FileExists(cheminBIN & ".bak") Then
            My.Computer.FileSystem.DeleteFile(cheminBIN & ".bak")
        End If

        posLecture = 0
        tailleBIN = FileLen(cheminBIN)
        tailleTampon = tailleBIN
        i = 50
        lectureBIN = New IO.FileStream(cheminBIN, IO.FileMode.Open, IO.FileAccess.Read, IO.FileShare.ReadWrite)

        compteur = 0
        nbSuppression = 0

        While posLecture < tailleBIN
            If posLecture + tailleTampon <= tailleBIN Then
                reservation = False
                Do
                    Try
                        ReDim tabBIN(tailleTampon - 1)
                        reservation = True
                    Catch ex As Exception
                        i = i - 1
                        tailleTampon = 24 * i * 1000000
                    End Try
                Loop Until reservation
                lectureBIN.Read(tabBIN, 0, tabBIN.Length)
            Else
                tailleTampon = tailleBIN - posLecture
                ReDim tabBIN(tailleTampon - 1)
                lectureBIN.Read(tabBIN, 0, tabBIN.Length)
            End If

            ReDim tabNEW(UBound(tabBIN))
            pos = 0
            offset = 0
            Do
                Array.Copy(tabBIN, pos, tabTampon, 0, 24)
                If profMin <= tabTampon(8) Then
                    Array.Copy(tabTampon, 0, tabNEW, offset * 24, 24)
                    offset = offset + 1
                Else
                    nbSuppression = nbSuppression + 1
                End If

                compteur = compteur + 1
                pos = pos + 24
            Loop While pos < tabBIN.Length

            tabBIN = Nothing

            ReDim Preserve tabNEW(offset * 24 - 1)
            My.Computer.FileSystem.WriteAllBytes(cheminBIN & ".bak", tabNEW, True)

            tabNEW = Nothing

            posLecture = lectureBIN.Position
        End While

        lectureBIN.Close()

        message = "info string " & nomFichier(cheminBIN) & " -> Total moves: " & compteur & ". Empty moves: " & nbSuppression & ". Fragmentation: " & Format(nbSuppression / compteur, "0.00%") & vbCrLf
        message = message & "info string Saved " & Format(compteur - nbSuppression, "0 moves") & " to " & nomFichier(cheminBIN) & " file"

        If compteur > nbSuppression Then
            My.Computer.FileSystem.DeleteFile(cheminBIN)
            My.Computer.FileSystem.RenameFile(cheminBIN & ".bak", nomFichier(cheminBIN))
        End If

        Return message
    End Function

    Public Function droite(texte As String, longueur As Integer) As String
        If longueur > 0 Then
            Return VB.Right(texte, longueur)
        Else
            Return ""
        End If
    End Function

    Public Sub echiquier(epd As String, e As PaintEventArgs, Optional coup1 As String = "", Optional coup2 As String = "")
        Dim tabEPD() As String, tabCaracteres() As Char
        Dim i As Integer, j As Integer, ligne As Integer, colonne As Integer
        Dim x As Integer, y As Integer, offsetGauche As Integer, offetHaut As Integer
        Dim largeur As Integer, hauteur As Integer, caseNoire As Boolean, chaine As String
        Dim trait As Char, stylo As System.Drawing.Pen, pinceau As System.Drawing.Brush
        Dim transparence As New ImageAttributes

        'minuscule = noirs
        'majuscule = blancs
        'w = trait blanc
        'b = trait noir
        '1-8 = cases vides

        'initialisation
        ligne = 1
        colonne = 1
        offsetGauche = -9
        offetHaut = -10
        largeur = 40 'grille
        hauteur = 40 'grille
        caseNoire = False 'en haut à gauche la case est blanche
        transparence.SetColorKey(Color.FromArgb(0, 255, 0), Color.FromArgb(0, 255, 0))

        'formatage
        For i = 2 To 8
            epd = Replace(epd, i, StrDup(i, "1"))
        Next

        'qui a le trait
        trait = ""
        If InStr(epd, " w ") > 0 Or InStr(epd, " b ") > 0 Then
            trait = epd.Chars(epd.IndexOf(" ") + 1)
        End If

        'dessiner la position de base
        tabEPD = Split(epd, "/")
        For i = 0 To UBound(tabEPD)
            If tabEPD(i) <> "" Then
                tabCaracteres = tabEPD(i).ToCharArray
                For j = 0 To UBound(tabCaracteres)
                    If tabCaracteres(j) = " " Then
                        Exit For
                    End If

                    x = colonne * largeur + offsetGauche  'centre case
                    y = ligne * hauteur + offetHaut 'centre case

                    'pièces
                    chaine = ""
                    Select Case tabCaracteres(j)
                        Case "r" 'rook = tour
                            chaine = "tourN"

                        Case "R"
                            chaine = "tourB"

                        Case "n" 'knight = cavalier
                            chaine = "cavalierN"

                        Case "N"
                            chaine = "cavalierB"

                        Case "b" 'bishop = fou
                            chaine = "fouN"

                        Case "B"
                            chaine = "fouB"

                        Case "q" 'queen = dame
                            chaine = "dameN"

                        Case "Q"
                            chaine = "dameB"

                        Case "k" 'king = roi
                            chaine = "roiN"

                        Case "K"
                            chaine = "roiB"

                        Case "p" 'pawn = pion
                            chaine = "pionN"

                        Case "P"
                            chaine = "pionB"

                    End Select

                    If chaine <> "" Then
                        e.Graphics.DrawImage(Image.FromFile("bmp\" & chaine & ".bmp"), New Rectangle(x - 14, y - 12, 37, 37), 0, 0, 37, 37, GraphicsUnit.Pixel, transparence)
                    End If

                    caseNoire = Not caseNoire
                    colonne = colonne + 1
                    If colonne = 9 Then
                        caseNoire = Not caseNoire
                        colonne = 1
                        ligne = ligne + 1
                    End If
                Next
            End If
        Next

        'coup moteur 1
        If coup1 <> "" And Not IsNothing(coup1) Then
            'moteurs d'accord ?
            If (Not IsNothing(coup2) And coup1 <> coup2) Or IsNothing(coup2) Then
                'non
                stylo = Pens.Blue
                pinceau = Brushes.Blue
            Else
                'oui
                stylo = Pens.Red
                pinceau = Brushes.Red
            End If

            echiquier_localisation(e, pinceau, stylo, coup1, tabEPD, trait, largeur, hauteur, offsetGauche)
        End If

        'coup moteur 2
        If coup2 <> "" And Not IsNothing(coup2) And ((Not IsNothing(coup1) And coup2 <> coup1) Or IsNothing(coup1)) Then
            stylo = Pens.Green
            pinceau = Brushes.Green

            echiquier_localisation(e, pinceau, stylo, coup2, tabEPD, trait, largeur, hauteur, offsetGauche)
        End If

        'trait
        If trait = "w" Then
            e.Graphics.FillEllipse(Brushes.White, New Rectangle(e.ClipRectangle.Width - 19, e.ClipRectangle.Height - 20, 15, 15))
            e.Graphics.DrawEllipse(Pens.Black, New Rectangle(e.ClipRectangle.Width - 19, e.ClipRectangle.Height - 20, 15, 15))
        ElseIf trait = "b" Then
            e.Graphics.FillEllipse(Brushes.Black, New Rectangle(e.ClipRectangle.Width - 19, 1, 15, 15))
            e.Graphics.DrawEllipse(Pens.Black, New Rectangle(e.ClipRectangle.Width - 19, 1, 15, 15))
        End If

        'totaux pièces
        e.Graphics.DrawString(epdTotalNoir(epd), New Font("courier new", 8), Brushes.Black, New Point(0, 0))
        e.Graphics.DrawString(epdTotalBlanc(epd), New Font("courier new", 8), Brushes.Black, New Point(0, e.ClipRectangle.Height - 16))

    End Sub

    Public Sub echiquier_localisation(e As PaintEventArgs, pinceau As System.Drawing.Brush, stylo As System.Drawing.Pen, coupMoteur As String, tabEPD() As String, trait As Char, largeur As Integer, hauteur As Integer, offsetGauche As Integer)
        Dim offsetRoque As Integer, x As Integer, y As Integer, colonne As Integer, ligne As Integer, xTrait As Integer, yTrait As Integer

        'rook
        offsetRoque = 7
        If coupMoteur = "e1g1" And tabEPD(7).Chars(4) = "K" And tabEPD(7).Chars(7) = "R" And trait = "w" Then
            '0-0
            'e1 => g1
            x = Val(Asc(coupMoteur.Chars(0)) - 96) * largeur + offsetGauche + 4
            y = (9 - Val(coupMoteur.Chars(1))) * hauteur - 4
            colonne = Val(Asc(coupMoteur.Chars(2)) - 96) * largeur + offsetGauche + 4
            ligne = (9 - Val(coupMoteur.Chars(3))) * hauteur - 4
            echiquier_dessin(e, pinceau, stylo, colonne - 4, ligne - 4 + offsetRoque, x + largeur / 2, y + offsetRoque, colonne, ligne + offsetRoque, x - largeur / 2, y - hauteur / 2, largeur, hauteur)

            'h1 => f1
            x = Val(Asc("h") - 96) * largeur + offsetGauche + 4
            y = (9 - 1) * hauteur - 4
            colonne = Val(Asc("f") - 96) * largeur + offsetGauche + 4
            ligne = (9 - 1) * hauteur - 4
            echiquier_dessin(e, pinceau, stylo, colonne - 4, ligne - 4 - offsetRoque, x - largeur / 2, y - offsetRoque, colonne, ligne - offsetRoque, x - largeur / 2, y - hauteur / 2, largeur, hauteur)

        ElseIf coupMoteur = "e1c1" And tabEPD(7).Chars(4) = "K" And tabEPD(7).Chars(0) = "R" And trait = "w" Then
            '0-0-0
            'e1 => c1
            x = Val(Asc(coupMoteur.Chars(0)) - 96) * largeur + offsetGauche + 4
            y = (9 - Val(coupMoteur.Chars(1))) * hauteur - 4
            colonne = Val(Asc(coupMoteur.Chars(2)) - 96) * largeur + offsetGauche + 4
            ligne = (9 - Val(coupMoteur.Chars(3))) * hauteur - 4
            echiquier_dessin(e, pinceau, stylo, colonne - 4, ligne - 4 + offsetRoque, x - largeur / 2, y + offsetRoque, colonne, ligne + offsetRoque, x - largeur / 2, y - hauteur / 2, largeur, hauteur)

            'a1 => d1
            x = Val(Asc("a") - 96) * largeur + offsetGauche + 4
            y = (9 - 1) * hauteur - 4
            colonne = Val(Asc("d") - 96) * largeur + offsetGauche + 4
            ligne = (9 - 1) * hauteur - 4
            echiquier_dessin(e, pinceau, stylo, colonne - 4, ligne - 4 - offsetRoque, x + largeur / 2, y - offsetRoque, colonne, ligne - offsetRoque, x - largeur / 2, y - hauteur / 2, largeur, hauteur)

        ElseIf coupMoteur = "e8g8" And tabEPD(0).Chars(4) = "k" And tabEPD(0).Chars(7) = "r" And trait = "b" Then
            '0-0
            'e8 => g8
            x = Val(Asc(coupMoteur.Chars(0)) - 96) * largeur + offsetGauche + 4
            y = (9 - Val(coupMoteur.Chars(1))) * hauteur - 4
            colonne = Val(Asc(coupMoteur.Chars(2)) - 96) * largeur + offsetGauche + 4
            ligne = (9 - Val(coupMoteur.Chars(3))) * hauteur - 4
            echiquier_dessin(e, pinceau, stylo, colonne - 4, ligne - 4 + offsetRoque, x + largeur / 2, y + offsetRoque, colonne, ligne + offsetRoque, x - largeur / 2, y - hauteur / 2, largeur, hauteur)

            'h8 => f8
            x = Val(Asc("h") - 96) * largeur + offsetGauche + 4
            y = (9 - 8) * hauteur - 4
            colonne = Val(Asc("f") - 96) * largeur + offsetGauche + 4
            ligne = (9 - 8) * hauteur - 4
            echiquier_dessin(e, pinceau, stylo, colonne - 4, ligne - 4 - offsetRoque, x - largeur / 2, y - offsetRoque, colonne, ligne - offsetRoque, x - largeur / 2, y - hauteur / 2, largeur, hauteur)

        ElseIf coupMoteur = "e8c8" And tabEPD(0).Chars(4) = "k" And tabEPD(0).Chars(0) = "r" And trait = "b" Then
            '0-0-0
            'e8 => c8
            x = Val(Asc(coupMoteur.Chars(0)) - 96) * largeur + offsetGauche + 4
            y = (9 - Val(coupMoteur.Chars(1))) * hauteur - 4
            colonne = Val(Asc(coupMoteur.Chars(2)) - 96) * largeur + offsetGauche + 4
            ligne = (9 - Val(coupMoteur.Chars(3))) * hauteur - 4
            echiquier_dessin(e, pinceau, stylo, colonne - 4, ligne - 4 + offsetRoque, x - largeur / 2, y + offsetRoque, colonne, ligne + offsetRoque, x - largeur / 2, y - hauteur / 2, largeur, hauteur)

            'a8 => d8
            x = Val(Asc("a") - 96) * largeur + offsetGauche + 4
            y = (9 - 8) * hauteur - 4
            colonne = Val(Asc("d") - 96) * largeur + offsetGauche + 4
            ligne = (9 - 8) * hauteur - 4
            echiquier_dessin(e, pinceau, stylo, colonne - 4, ligne - 4 - offsetRoque, x + largeur / 2, y - offsetRoque, colonne, ligne - offsetRoque, x - largeur / 2, y - hauteur / 2, largeur, hauteur)

        Else
            'coup simple
            x = Val(Asc(coupMoteur.Chars(0)) - 96) * largeur + offsetGauche + 4
            y = (9 - Val(coupMoteur.Chars(1))) * hauteur - 4
            colonne = Val(Asc(coupMoteur.Chars(2)) - 96) * largeur + offsetGauche + 4
            ligne = (9 - Val(coupMoteur.Chars(3))) * hauteur - 4
            xTrait = 0
            yTrait = 0
            offsetRoque = 0

            'traits
            If x = colonne Then
                'même colonne
                If y > ligne Then
                    'vers le haut
                    xTrait = x
                    yTrait = y - hauteur / 2
                Else
                    'vers le bas
                    xTrait = x
                    yTrait = y + hauteur / 2
                End If
            ElseIf y = ligne Then
                'même ligne
                If x < colonne Then
                    'vers la droite
                    xTrait = x + largeur / 2
                    yTrait = y
                Else
                    'vers la gauche
                    xTrait = x - largeur / 2
                    yTrait = y
                End If
            ElseIf colonne > x And ligne < y Then
                'vers le haut et vers la droite
                xTrait = x + largeur / 2
                yTrait = y - hauteur / 2
            ElseIf colonne < x And ligne < y Then
                'vers le haut et vers la gauche
                xTrait = x - largeur / 2
                yTrait = y - hauteur / 2
            ElseIf colonne > x And ligne > y Then
                'vers le bas et vers la droite
                xTrait = x + largeur / 2
                yTrait = y + hauteur / 2
            ElseIf colonne < x And ligne > y Then
                'vers le bas et vers la gauche
                xTrait = x - largeur / 2
                yTrait = y + hauteur / 2
            End If

            echiquier_dessin(e, pinceau, stylo, colonne - 4, ligne - 4, xTrait, yTrait, colonne, ligne, x - largeur / 2, y - hauteur / 2, largeur, hauteur)

        End If
    End Sub

    Public Sub echiquier_dessin(e As PaintEventArgs, pinceau As System.Drawing.Brush, stylo As System.Drawing.Pen, xRond As Integer, yRond As Integer, xTrait As Integer, yTrait As Integer, x1Trait As Integer, y1Trait As Integer, xContour As Integer, yContour As Integer, largContour As Integer, hautContour As Integer)
        'ronds
        e.Graphics.FillEllipse(pinceau, New Rectangle(xRond, yRond, 8, 8))

        'trait
        e.Graphics.DrawLine(stylo, New Point(xTrait, yTrait), New Point(x1Trait, y1Trait))

        'contours
        e.Graphics.DrawRectangle(stylo, New Rectangle(xContour, yContour, largContour, hautContour))
        e.Graphics.DrawRectangle(stylo, New Rectangle(xContour + 1, yContour + 1, largContour - 2, hautContour - 2))
        e.Graphics.DrawRectangle(stylo, New Rectangle(xContour + 2, yContour + 2, largContour - 4, hautContour - 4))

    End Sub

    Public Sub echiquier_differences(epd As String, e As PaintEventArgs)
        Dim i As Integer, piecesNoires As String, piecesBlanches As String, tmp() As Char
        Dim chaine As String, transparence As New ImageAttributes
        Dim pasN As Integer, pasB As Integer, larg As Integer

        larg = 20
        transparence.SetColorKey(Color.FromArgb(0, 255, 0), Color.FromArgb(0, 255, 0))

        'formatage
        For i = 1 To 8
            epd = Replace(epd, i, "")
        Next
        epd = Replace(epd, "/", "")
        If epd = "" Then
            Exit Sub
        End If
        epd = epd.Substring(0, epd.IndexOf(" "))

        'on liste les pièces noires
        piecesNoires = ""
        piecesBlanches = ""
        tmp = epd.ToCharArray
        For i = 0 To UBound(tmp)
            Select Case tmp(i)
                Case "r" 'rook = tour
                    piecesNoires = piecesNoires & tmp(i)

                Case "R"
                    piecesBlanches = piecesBlanches & tmp(i)

                Case "n" 'knight = cavalier
                    piecesNoires = piecesNoires & tmp(i)

                Case "N"
                    piecesBlanches = piecesBlanches & tmp(i)

                Case "b" 'bishop = fou
                    piecesNoires = piecesNoires & tmp(i)

                Case "B"
                    piecesBlanches = piecesBlanches & tmp(i)

                Case "q" 'queen = dame
                    piecesNoires = piecesNoires & tmp(i)

                Case "Q"
                    piecesBlanches = piecesBlanches & tmp(i)

                Case "p" 'pawn = pion
                    piecesNoires = piecesNoires & tmp(i)

                Case "P"
                    piecesBlanches = piecesBlanches & tmp(i)

            End Select
        Next

        'on supprime chaque pièce noir dans la liste des blanches
        tmp = piecesNoires.ToCharArray
        For i = 0 To UBound(tmp)
            If InStr(piecesBlanches, tmp(i), CompareMethod.Text) > 0 Then
                piecesBlanches = Replace(piecesBlanches, tmp(i), "", , 1, CompareMethod.Text)
                piecesNoires = Replace(piecesNoires, tmp(i), "", , 1)
            End If
        Next

        'on trie les pièces par valeur
        If piecesBlanches <> "" Then
            piecesBlanches = Replace(piecesBlanches, "Q", "1")
            piecesBlanches = Replace(piecesBlanches, "R", "2")
            piecesBlanches = Replace(piecesBlanches, "B", "3")
            piecesBlanches = Replace(piecesBlanches, "N", "4")
            piecesBlanches = Replace(piecesBlanches, "P", "5")
            'chaine vers tableau => tri alphabétique => tableau vers chaine
            tmp = piecesBlanches.ToCharArray
            Array.Sort(tmp)
            piecesBlanches = New String(tmp)
            'on met en forme la chaine
            piecesBlanches = Replace(piecesBlanches, "1", "Q")
            piecesBlanches = Replace(piecesBlanches, "2", "R")
            piecesBlanches = Replace(piecesBlanches, "3", "B")
            piecesBlanches = Replace(piecesBlanches, "4", "N")
            piecesBlanches = Replace(piecesBlanches, "5", "P")
        End If

        'on trie les pièces par valeur
        If piecesNoires <> "" Then
            piecesNoires = Replace(piecesNoires, "q", "1")
            piecesNoires = Replace(piecesNoires, "r", "2")
            piecesNoires = Replace(piecesNoires, "b", "3")
            piecesNoires = Replace(piecesNoires, "n", "4")
            piecesNoires = Replace(piecesNoires, "p", "5")
            'chaine vers tableau => tri alphabétique => tableau vers chaine
            tmp = piecesNoires.ToCharArray
            Array.Sort(tmp)
            piecesNoires = New String(tmp)
            'on met en forme la chaine
            piecesNoires = Replace(piecesNoires, "1", "q")
            piecesNoires = Replace(piecesNoires, "2", "r")
            piecesNoires = Replace(piecesNoires, "3", "b")
            piecesNoires = Replace(piecesNoires, "4", "n")
            piecesNoires = Replace(piecesNoires, "5", "p")
        End If

        'on cumule les pièces manquantes (noires à gauche, blanches à droite)
        chaine = piecesNoires & piecesBlanches

        If chaine = "" Then
            Exit Sub
        End If

        'pièces
        tmp = chaine.ToCharArray
        pasN = -1
        pasB = -1
        For i = 0 To UBound(tmp)
            Select Case tmp(i)
                Case "r" 'rook = tour
                    chaine = "tourN"

                Case "R"
                    chaine = "tourB"

                Case "n" 'knight = cavalier
                    chaine = "cavalierN"

                Case "N"
                    chaine = "cavalierB"

                Case "b" 'bishop = fou
                    chaine = "fouN"

                Case "B"
                    chaine = "fouB"

                Case "q" 'queen = dame
                    chaine = "dameN"

                Case "Q"
                    chaine = "dameB"

                Case "p" 'pawn = pion
                    chaine = "pionN"

                Case "P"
                    chaine = "pionB"

            End Select

            If chaine <> "" Then
                If InStr(chaine, "N") = Len(chaine) Then
                    pasN = pasN + 1
                    e.Graphics.DrawImage(Image.FromFile("bmp\" & chaine & ".bmp"), New Rectangle(15 + pasN * larg, 1, larg, larg), 0, 0, 37, 37, GraphicsUnit.Pixel, transparence)
                ElseIf InStr(chaine, "B") = Len(chaine) Then
                    pasB = pasB + 1
                    e.Graphics.DrawImage(Image.FromFile("bmp\" & chaine & ".bmp"), New Rectangle(e.ClipRectangle.Width / 2 + pasB * larg, 1, larg, larg), 0, 0, 37, 37, GraphicsUnit.Pixel, transparence)
                End If
            End If
        Next

    End Sub

    Public Function epdTotalBlanc(fen As String) As Integer
        Dim tabCaracteres() As Char, i As Integer, total As Integer

        'initialisation
        total = 0

        tabCaracteres = fen.ToCharArray
        For i = 0 To UBound(tabCaracteres)
            If tabCaracteres(i) = " " Then
                Exit For
            End If

            'pièces
            Select Case tabCaracteres(i)
                Case "R"
                    total = total + 5

                Case "N"
                    total = total + 3

                Case "B"
                    total = total + 3

                Case "Q"
                    total = total + 9

                Case "P"
                    total = total + 1

            End Select
        Next

        Return total

    End Function

    Public Function epdTotalNoir(fen As String) As Integer
        Dim tabCaracteres() As Char, i As Integer, total As Integer

        'initialisation
        total = 0

        tabCaracteres = fen.ToCharArray
        For i = 0 To UBound(tabCaracteres)
            If tabCaracteres(i) = " " Then
                Exit For
            End If

            'pièces
            Select Case tabCaracteres(i)
                Case "r"
                    total = total + 5

                Case "n"
                    total = total + 3

                Case "b"
                    total = total + 3

                Case "q"
                    total = total + 9

                Case "p"
                    total = total + 1

            End Select
        Next

        Return total

    End Function

    Public Function epdToEXP(entreeDefrag As System.IO.StreamWriter, sortieDefrag As System.IO.StreamReader, Optional startpos As String = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") As Byte()
        Dim key As String

        entreedefrag.WriteLine("position fen " & startpos)

        entreedefrag.WriteLine("d")

        key = ""
        While InStr(key, "Key: ", CompareMethod.Text) = 0
            key = sortiedefrag.ReadLine
        End While

        key = Replace(key, "Key: ", "")

        '6078880BD90221F8 =>  F8 21 02  D9 0B  88  78 60
        '                    248 33  2 217 11 136 120 96

        Return inverseurHEX(key, 8)

    End Function

    Public Function extensionFichier(chemin As String) As String
        Dim fichier As New FileInfo(chemin)
        Return fichier.Extension
    End Function

    Public Function formaterInfos(info As String, position As String) As String
        Dim tabChaine() As String, i As Integer, coup As String
        Dim prof As String, score As Single, temps As String, mat As String

        'de ça : info depth 14 score cp 7148 time 30 pv g5g7
        'vers ça : g5g7 {7148/14 30}

        prof = ""
        score = -1000000
        temps = ""
        coup = ""
        mat = ""
        tabChaine = Split(info, " ")
        For i = 0 To UBound(tabChaine)
            If tabChaine(i) = "depth" Then
                prof = tabChaine(i + 1)
            ElseIf tabChaine(i) = "score" And tabChaine(i + 1) = "cp" Then '7148 cp
                score = CSng(CInt(tabChaine(i + 2)) / 100) '71.48
                If score <> 0 And InStr(position, " b ") > 0 Then
                    score = -score
                End If
            ElseIf tabChaine(i) = "score" And tabChaine(i + 1) = "mate" Then
                mat = "mate "
                score = CInt(tabChaine(i + 2))
                If InStr(position, " b ") > 0 Then
                    score = -score
                End If
            ElseIf tabChaine(i) = "time" Then
                temps = CInt(CInt(tabChaine(i + 1)) / 1000)
            ElseIf tabChaine(i) = "pv" Then
                coup = tabChaine(i + 1)
            End If
            If score <> -1000000 And prof <> "" And temps <> "" And coup <> "" Then
                Exit For
            End If
        Next

        If mat <> "" Then
            Return LCase(coup) & " {" & mat & Format(score, "0") & "/" & prof & " " & temps & "}"
        Else
            Return LCase(coup) & " {" & mat & Replace(Format(score, "0.00"), ",", ".") & "/" & prof & " " & temps & "}"
        End If
    End Function

    Public Function gauche(texte As String, longueur As Integer) As String
        If longueur > 0 Then
            Return VB.Left(texte, longueur)
        Else
            Return ""
        End If
    End Function

    Public Function hexa(valeur As Integer) As String
        Dim chaine As String

        chaine = Hex(valeur)
        If Len(chaine) = 1 Then
            chaine = "0" & chaine
        End If
        Return chaine

    End Function

    Public Function hexadecimalToBinaire(hexadecimal As String) As String
        Dim entier As Integer, chaine As String

        entier = Convert.ToInt64(hexadecimal, 16)
        chaine = ""

        Do
            chaine = Format(entier Mod 2) & chaine
            entier = Fix(entier / 2)
        Loop While (entier / 2 > 0)
        chaine = CDbl(chaine).ToString(StrDup(4 * Len(hexadecimal), "0"))
        Return chaine
    End Function

    Public Function inverseurHEX(chaine As String, taille As Integer) As Byte()
        Dim i As Integer, index As Integer, tab(taille - 1) As Byte

        index = 0
        For i = Len(chaine) To 2 Step -2
            tab(index) = Convert.ToInt64(droite(gauche(chaine, i), 2), 16)
            index = index + 1
        Next

        Return tab
    End Function

    Public Function maxMultiPVMoteur(chaine As String) As Integer
        maxMultiPVMoteur = 200
        If InStr(chaine, "asmfish", CompareMethod.Text) > 0 Then
            maxMultiPVMoteur = 224
        ElseIf InStr(chaine, "brainfish", CompareMethod.Text) > 0 _
            Or InStr(chaine, "brainlearn", CompareMethod.Text) > 0 _
            Or InStr(chaine, "stockfish", CompareMethod.Text) > 0 _
            Or InStr(chaine, "cfish", CompareMethod.Text) > 0 _
            Or InStr(chaine, "sugar", CompareMethod.Text) > 0 _
            Or InStr(chaine, "eman", CompareMethod.Text) > 0 _
            Or InStr(chaine, "hypnos", CompareMethod.Text) > 0 _
            Or InStr(chaine, "judas", CompareMethod.Text) > 0 _
            Or InStr(chaine, "aurora", CompareMethod.Text) > 0 Then
            maxMultiPVMoteur = 500
        ElseIf InStr(chaine, "houdini", CompareMethod.Text) > 0 Then
            maxMultiPVMoteur = 220
        ElseIf InStr(chaine, "komodo", CompareMethod.Text) > 0 Then
            maxMultiPVMoteur = 218
        End If

        Return maxMultiPVMoteur
    End Function

    Public Function moteurEPD(moteur As String, moves As String, Optional startpos As String = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") As String
        Dim processusEPD As New System.Diagnostics.Process(), entreeEPD As System.IO.StreamWriter, sortieEPD As System.IO.StreamReader, chaineEPD As String

        chaineEPD = startpos
        If moves <> "" Then
            'on charge le moteur
            processusEPD.StartInfo.RedirectStandardOutput = True
            processusEPD.StartInfo.UseShellExecute = False
            processusEPD.StartInfo.RedirectStandardInput = True
            processusEPD.StartInfo.RedirectStandardError = True
            processusEPD.StartInfo.CreateNoWindow = True
            processusEPD.StartInfo.FileName = moteur
            processusEPD.Start()

            entreeEPD = processusEPD.StandardInput
            sortieEPD = processusEPD.StandardOutput

            entreeEPD.WriteLine("position fen " & startpos & " moves " & moves)

            entreeEPD.WriteLine("d")

            chaineEPD = ""
            While InStr(chaineEPD, "Fen: ", CompareMethod.Text) = 0
                chaineEPD = sortieEPD.ReadLine
                Application.DoEvents()
            End While
            entreeEPD.WriteLine("quit")

            entreeEPD.Close()
            sortieEPD.Close()
            processusEPD.Close()

        End If

        entreeEPD = Nothing
        sortieEPD = Nothing
        processusEPD = Nothing

        Return Replace(chaineEPD, "Fen: ", "")
    End Function

    Public Function moveToBIN(coup As String, Optional litteral As Boolean = False) As Byte()
        Dim coupBIN As String, i As Integer, cumul As Integer, coupHEX As String

        'g8h6
        '0 000 111(8) 110(g) 101(6) 111(h)

        coupBIN = ""

        If Not litteral Then
            If coup = "e1c1" Then
                coupBIN = "1 100 000 100 000 000" 'equivalent e1a1

            ElseIf coup = "e8c8" Then
                coupBIN = "1 100 111 100 111 000" 'equivalent e8a8

            ElseIf coup = "e1g1" Then
                coupBIN = "1 100 000 100 000 111" 'equivalent e1h1

            ElseIf coup = "e8g8" Then
                coupBIN = "1 100 111 100 111 111" 'equivalent e8h8

            End If
        End If

        If coupBIN = "" Then
            coupBIN = "0 000 "
            If Len(coup) = 5 Then
                Select Case droite(coup, 1)
                    Case "N", "n"
                        coupBIN = "0 001 "
                    Case "B", "b"
                        coupBIN = "0 101 "
                    Case "R", "r"
                        coupBIN = "0 110 "
                    Case "Q", "q"
                        coupBIN = "0 111 "
                End Select
            End If

            'ligne de départ
            If coup.Substring(1, 1) = "1" Then
                coupBIN = coupBIN & "000" & " "
            ElseIf coup.Substring(1, 1) = "2" Then
                coupBIN = coupBIN & "001" & " "
            ElseIf coup.Substring(1, 1) = "3" Then
                coupBIN = coupBIN & "010" & " "
            ElseIf coup.Substring(1, 1) = "4" Then
                coupBIN = coupBIN & "011" & " "
            ElseIf coup.Substring(1, 1) = "5" Then
                coupBIN = coupBIN & "100" & " "
            ElseIf coup.Substring(1, 1) = "6" Then
                coupBIN = coupBIN & "101" & " "
            ElseIf coup.Substring(1, 1) = "7" Then
                coupBIN = coupBIN & "110" & " "
            ElseIf coup.Substring(1, 1) = "8" Then
                coupBIN = coupBIN & "111" & " "
            End If

            'colonne de départ
            If coup.Substring(0, 1) = "a" Then
                coupBIN = coupBIN & "000" & " "
            ElseIf coup.Substring(0, 1) = "b" Then
                coupBIN = coupBIN & "001" & " "
            ElseIf coup.Substring(0, 1) = "c" Then
                coupBIN = coupBIN & "010" & " "
            ElseIf coup.Substring(0, 1) = "d" Then
                coupBIN = coupBIN & "011" & " "
            ElseIf coup.Substring(0, 1) = "e" Then
                coupBIN = coupBIN & "100" & " "
            ElseIf coup.Substring(0, 1) = "f" Then
                coupBIN = coupBIN & "101" & " "
            ElseIf coup.Substring(0, 1) = "g" Then
                coupBIN = coupBIN & "110" & " "
            ElseIf coup.Substring(0, 1) = "h" Then
                coupBIN = coupBIN & "111" & " "
            End If

            'ligne d'arrivée
            If coup.Substring(3, 1) = "1" Then
                coupBIN = coupBIN & "000" & " "
            ElseIf coup.Substring(3, 1) = "2" Then
                coupBIN = coupBIN & "001" & " "
            ElseIf coup.Substring(3, 1) = "3" Then
                coupBIN = coupBIN & "010" & " "
            ElseIf coup.Substring(3, 1) = "4" Then
                coupBIN = coupBIN & "011" & " "
            ElseIf coup.Substring(3, 1) = "5" Then
                coupBIN = coupBIN & "100" & " "
            ElseIf coup.Substring(3, 1) = "6" Then
                coupBIN = coupBIN & "101" & " "
            ElseIf coup.Substring(3, 1) = "7" Then
                coupBIN = coupBIN & "110" & " "
            ElseIf coup.Substring(3, 1) = "8" Then
                coupBIN = coupBIN & "111" & " "
            End If

            'colonne d'arrivée
            If coup.Substring(2, 1) = "a" Then
                coupBIN = coupBIN & "000"
            ElseIf coup.Substring(2, 1) = "b" Then
                coupBIN = coupBIN & "001"
            ElseIf coup.Substring(2, 1) = "c" Then
                coupBIN = coupBIN & "010"
            ElseIf coup.Substring(2, 1) = "d" Then
                coupBIN = coupBIN & "011"
            ElseIf coup.Substring(2, 1) = "e" Then
                coupBIN = coupBIN & "100"
            ElseIf coup.Substring(2, 1) = "f" Then
                coupBIN = coupBIN & "101"
            ElseIf coup.Substring(2, 1) = "g" Then
                coupBIN = coupBIN & "110"
            ElseIf coup.Substring(2, 1) = "h" Then
                coupBIN = coupBIN & "111"
            End If
        End If

        '0 000 111(8) 110(g) 101(6) 111(h)
        coupBIN = Replace(coupBIN, " ", "")

        '0000111110101111
        cumul = 0
        For i = 1 To Len(coupBIN)
            cumul = cumul + CInt(gauche(droite(coupBIN, i), 1)) * 2 ^ (i - 1)
        Next
        coupHEX = Hex(cumul)
        coupHEX = StrDup(CInt(Len(coupBIN) / 4 - Len(coupHEX)), "0") & coupHEX

        '0000(0) 1111(F) 1010(A) 1111(F)
        'AF(175) OF(15)

        Return inverseurHEX(coupHEX, 2)
    End Function

    Public Function nbCaracteres(ByVal chaine As String, ByVal critere As String) As Integer
        Return Len(chaine) - Len(Replace(chaine, critere, ""))
    End Function

    Public Function nomFichier(chemin As String) As String
        Return My.Computer.FileSystem.GetName(chemin)
    End Function

    Public Function optionsUCI(entreeOPT As System.IO.StreamWriter, sortieOPT As System.IO.StreamReader) As String
        Dim chaine As String, ligne As String, nom As String

        entreeOPT.WriteLine("uci")

        entreeOPT.WriteLine("isready")

        chaine = ""
        ligne = ""
        While InStr(ligne, "readyok", CompareMethod.Text) = 0
            ligne = sortieOPT.ReadLine
            If gauche(ligne, 12) = "option name " And InStr(ligne, " default ", CompareMethod.Text) > 0 Then
                nom = ""
                ligne = Replace(ligne, "option name ", "")
                If InStr(ligne, " type string ") > 0 Then
                    nom = gauche(ligne, ligne.IndexOf(" type string "))
                ElseIf InStr(ligne, " type spin ") > 0 Then
                    nom = gauche(ligne, ligne.IndexOf(" type spin "))
                ElseIf InStr(ligne, " type check ") > 0 Then
                    nom = gauche(ligne, ligne.IndexOf(" type check "))
                ElseIf InStr(ligne, " type combo ") > 0 Then
                    nom = gauche(ligne, ligne.IndexOf(" type combo "))
                End If
                chaine = chaine & nom & "|" & Replace(Replace(Replace(ligne.Substring(ligne.IndexOf(" default ") + 9), " min ", "|"), " max ", "|"), " var ", "|") & vbCrLf
            End If
        End While

        Return chaine
    End Function

    Public Sub pgnUCI(chemin As String, fichier As String, suffixe As String, Optional priorite As Integer = 64)
        Dim nom As String, commande As New Process()
        Dim dossierFichier As String, dossierTravail As String

        nom = Replace(nomFichier(fichier), ".pgn", "")

        dossierFichier = fichier.Substring(0, fichier.LastIndexOf("\"))
        dossierTravail = My.Computer.FileSystem.GetParentPath(chemin)

        'si pgn-extract.exe ne se trouve à l'emplacement prévu (par <nom_ordinateur>.ini)
        If Not My.Computer.FileSystem.FileExists(dossierTravail & "\pgn-extract.exe") Then

            'si pgn-extract.exe ne se trouve dans le même dossier que le notre application
            dossierTravail = Environment.CurrentDirectory
            If Not My.Computer.FileSystem.FileExists(dossierTravail & "\pgn-extract.exe") Then

                'on cherche s'il se trouve dans le même dossier que le fichierPGN
                dossierTravail = dossierFichier
                If Not My.Computer.FileSystem.FileExists(dossierTravail & "\pgn-extract.exe") Then

                    'pgn-extract.exe est introuvable
                    MsgBox("Veuillez copier pgn-extract.exe dans :" & vbCrLf & dossierTravail, MsgBoxStyle.Critical)
                    dossierTravail = Environment.CurrentDirectory
                    If Not My.Computer.FileSystem.FileExists(dossierTravail & "\pgn-extract.exe") Then
                        End
                    End If
                End If
            End If

        End If

        'si le fichierPGN ne se trouve pas dans le dossier de travail
        If dossierFichier <> dossierTravail Then
            'on recopie temporairement le fichierPGN dans le dossierTravail
            My.Computer.FileSystem.CopyFile(fichier, dossierTravail & "\" & nom & ".pgn", True)
        End If

        commande.StartInfo.FileName = dossierTravail & "\pgn-extract.exe"
        commande.StartInfo.WorkingDirectory = dossierTravail

        If InStr(nom, " ") = 0 Then
            commande.StartInfo.Arguments = " -s -Wuci -o" & nom & suffixe & ".pgn" & " " & nom & ".pgn"
        Else
            commande.StartInfo.Arguments = " -s -Wuci -o""" & nom & suffixe & ".pgn""" & " """ & nom & ".pgn"""
        End If

        commande.StartInfo.CreateNoWindow = True
        commande.StartInfo.UseShellExecute = False
        commande.Start()
        commande.PriorityClass = priorite '64 (idle), 16384 (below normal), 32 (normal), 32768 (above normal), 128 (high), 256 (realtime)
        commande.WaitForExit()

        'si le dossierTravail ne correspond pas au dossier du fichierPGN
        If dossierFichier <> dossierTravail Then
            'on déplace le fichier moteur
            Try
                My.Computer.FileSystem.DeleteFile(dossierTravail & "\" & nom & ".pgn")
            Catch ex As Exception

            End Try
            My.Computer.FileSystem.MoveFile(dossierTravail & "\" & nom & suffixe & ".pgn", dossierFichier & "\" & nom & suffixe & ".pgn")
        End If

    End Sub

    Public Function trierChaine(serie As String, separateur As String, Optional ordre As Boolean = True) As String
        Dim tabChaine() As String

        tabChaine = Split(serie, separateur)
        If tabChaine(UBound(tabChaine)) = "" Then
            ReDim Preserve tabChaine(UBound(tabChaine) - 1)
        End If

        Array.Sort(tabChaine)
        If Not ordre Then
            Array.Reverse(tabChaine)
        End If

        Return String.Join(separateur, tabChaine)
    End Function

    Public Function uciKEY(entreeKEY As System.IO.StreamWriter, sortieKEY As System.IO.StreamReader, movesUCI As String, Optional startpos As String = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") As String
        Dim key As String

        key = startpos

        If movesUCI <> "" Then
            entreeKEY.WriteLine("position fen " & startpos & " moves " & movesUCI)
        Else
            entreeKEY.WriteLine("position fen " & startpos)
        End If

        entreeKEY.WriteLine("d")

        key = ""
        While InStr(key, "Key: ", CompareMethod.Text) = 0
            key = sortieKEY.ReadLine
        End While

        Return Replace(key, "Key: ", "")
    End Function

End Module
