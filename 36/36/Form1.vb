Public Class Form1
    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        Dim n As Integer = InputBox("請輸入數字", "01", "1")
        Dim a As Long = 1
        For i = 1 To n
            a = a * i
        Next
        MsgBox(a)
    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        Dim n As String = InputBox("請輸入數字", "02", "1 2 3 4 5 6 7")
        Dim nn() As String = Split(n, " ")
        Dim a As String
        Dim c As Integer
        For i = 0 To 6
            c = nn(i)
            a &= StrDup(c, "*") & vbCrLf
        Next
        MsgBox(a)
    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        Dim n As String = InputBox("請輸入數字", "03", "100")
        Dim m0 As Integer = n
        Dim m1, m2 As String
        m1 = Convert.ToString(m0, 8)
        m2 = Convert.ToString(m0, 16)
        MsgBox(m1, m2)
    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        Dim n As Decimal = InputBox("請輸入1到9的數字", "04", "1")
        Dim m As String = InputBox("請輸入1到9的數字", "04", "1")
        Dim s As Decimal
        For a = 1 To 9
            s = n * 10 + m + n
        Next
        MsgBox(s)
    End Sub

    Private Sub Button5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button5.Click
        Dim n As Decimal = MsgBox("四位數 0 到 7 的奇數")
        Dim x As Integer
        Dim a As Integer = 2
        Dim s As Integer = CInt(n) Mod a
        Do While s > 0
            x = 1 + x
        Loop
        MsgBox(x)
    End Sub

    Private Sub Button6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button6.Click
        Dim n As Integer = MsgBox("809*??=800*??+9*??")
        For a = 10 To 99
            If 8 * a < 100 And 9 * a >= 100 Then
                MsgBox(a)
            End If
        Next
    End Sub

    Private Sub Button7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button7.Click
        Dim n As String = InputBox("請輸入數字", "07", "6666")
        Dim a As Integer = Mid(n, 1, 1)
        Dim a1 As Integer = Mid(n, 1, 2)
        Dim a2 As Integer = Mid(n, 1, 3)
        Dim a3 As Integer = Mid(n, 1, 4)
        Dim x As String
        x = ((a3 + 5) Mod 10) & ((a2 + 5) Mod 10) & ((a1 + 5) Mod 10) & ((a + 5) Mod 10)
        MsgBox(x)
    End Sub

    Private Sub Button8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button8.Click
        Dim a As Integer = 0
        Dim s As String
        Dim count As Integer
        For a = 1 To 4
            For a1 = 1 To 4
                For a2 = 1 To 4
                    If a <> a1 And a1 <> a2 And a2 <> a Then
                        count += 1
                        s &= a & a1 & a2 & vbNewLine
                    End If

                Next
            Next
        Next
        MsgBox(count)
        MsgBox(s)
    End Sub

    Private Sub Button9_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button9.Click
        Dim n As String = InputBox("請輸入金額", "09", "1000000")
        Dim a As Long
        Select Case n
            Case Is > 1000000
                a = ((n - 1000000) / 100) + ((n - 600000) * 0.015) + ((n - 400000) * 0.03) + ((n - 200000) * 0.05) + ((n - 100000) * 0.075)
            Case Is > 600000
                a = ((n - 600000) * 0.015) + ((n - 400000) * 0.03) + ((n - 200000) * 0.05) + ((n - 100000) * 0.075)
            Case Is > 400000
                a = ((n - 40) * 0.03) + ((n - 200000) * 0.05) + ((n - 100000) * 0.075)
            Case Is > 200000
                a = ((n - 200000) * 0.05) + ((n - 100000) * 0.075)
            Case Is > 100000
                a = ((n - 100000) * 0.075)
            Case Is < 100000
                a = (n * 0.1)
        End Select
        a = Int(a + 0.5)
        MsgBox(a)
    End Sub

    Private Sub Button10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button10.Click
        Dim ans As Integer
        For h = 1 To 100000
            If Math.Sqrt(h + 100) = Int(Math.Sqrt(h + 100)) And Math.Sqrt(h + 268) = Int(Math.Sqrt(h + 268)) Then MsgBox(h)
        Next
    End Sub

    Private Sub Button11_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button11.Click
        Dim years As String = InputBox("請輸入年分", "11", 87)
        Dim days As String = InputBox("請輸入日期", "11", "0807")
        Dim year As String
        Dim mounth As Integer = Mid(days, 1, 2)
        Dim day As Integer = Mid(days, 3, 2)
        Dim ans As Integer
        Select Case mounth
            Case Is = 1
                day = 0
            Case Is = 2
                day = 31
            Case Is = 3
                day = 59
            Case Is = 4
                day = 90
            Case Is = 5
                day = 120
            Case Is = 6
                day = 151
            Case Is = 7
                day = 181
            Case Is = 8
                day = 212
            Case Is = 9
                day = 243
            Case Is = 10
                day = 273
            Case Is = 11
                day = 304
            Case Is = 12
                day = 335
        End Select
        If mounth >= 3 And Int(year / 4) = year / 4 And Int(year / 100) <> year / 100 Then ans += 1
        ans = day + mounth
        MsgBox(ans)
    End Sub

    Private Sub Button12_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button12.Click

    End Sub '*

    Private Sub Button13_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button13.Click
        Dim c As Integer
        Dim ans As String
        For a = 1 To 9
            For b = 1 To 9
                c = a * b
                If b = 9 Then ans &= vbNewLine
                ans &= a & "*" & b & "=" & c & "  "
            Next
        Next
        MsgBox(ans)
    End Sub

    Private Sub Button14_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button14.Click
        Dim n As String = InputBox("請輸入一個小於五位數的數字", "14", "12345")
        Dim number As String = Len(n)
        Dim unnumber As Integer = StrReverse(n)
        For i = 0 To 99999

        Next
        MsgBox(number)
        MsgBox(unnumber)
    End Sub

    Private Sub Button15_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button15.Click
        Dim rabbit(15) As Integer
        rabbit(1) = 1
        Dim ans As String
        For i = 2 To 15
            rabbit(i) = rabbit(i - 1) + rabbit(i - 2)
            ans &= "第" & i & "個月生" & rabbit(i) & "隻兔子" & vbNewLine
        Next
        MsgBox(ans)
    End Sub

    Private Sub Button16_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button16.Click
        Dim n As String = InputBox("請輸入一個不是質數的偶數", "16", "16")
        Dim m As Integer
        Dim ANS As String
        Dim b As Integer
        Dim b1 As Integer
        For a = 1 To m
            If m Mod a = 0 Then ANS = "你輸入的數值是質數" Else ANS &= b & "+" & b1 & "=" & m
            For b = 1 To m
                'm-1 是質數? 不是就再-1
            Next
        Next
    End Sub '*

    Private Sub Button17_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button17.Click
        Dim k As Boolean
        Dim ans As String
        For n = 2 To 100
            k = True
            For m = 2 To n ^ 0.5
                If n Mod m = 0 Then
                    k = False
                    Exit For
                End If
            Next
            If k Then
                ans &= n & vbNewLine
            End If
        Next
        MsgBox(ans)
    End Sub

    Private Sub Button18_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button18.Click
        Dim k As Boolean
        Dim ans As String
        Dim count As String
        For n = 101 To 200
            k = True
            For m = 101 To n ^ 0.5
                If n Mod m = 0 Then
                    k = False
                    Exit For
                End If
            Next
            If k Then
                count += 1
                ans &= n & "  "
            End If
        Next
        MsgBox(count)
        MsgBox(ans)
    End Sub

    Private Sub Button19_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button19.Click
        Dim ans As String
        For b = 1 To 9
            For b1 = 0 To 9
                For b2 = 0 To 9
                    If (b & b1 & b2) = ((b ^ 3) + (b1 ^ 3) + (b2 ^ 3)) Then ans &= b & b1 & b2 & "  "
                Next
            Next
        Next
        MsgBox(ans)
    End Sub

    Private Sub Button20_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button20.Click
        Dim n As String = InputBox("請輸入一個數字", "20", "90")
        Dim x As Integer = n
        Dim ans As String = x & "="
        Dim i As Integer = 2
        Do Until i = 3
            Do While x <> 1
                If x Mod i = 0 Then
                    x = x / i
                    ans = ans & i & "*"
                Else
                    i = i + 1
                End If
            Loop
            MsgBox(ans)
            Exit Do
        Loop
    End Sub

    Private Sub Button21_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button21.Click
        Dim grade As Integer = InputBox("請輸入成績", "21", "90")
        Select Case grade
            Case Is >= 90
                MsgBox("A")
            Case Is >= 60
                MsgBox("B")
            Case Else
                MsgBox("C")
        End Select
    End Sub

    Private Sub Button22_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button22.Click
        Dim m As Integer = InputBox("請輸入第一個數字", "22", "25")
        Dim m1 As Integer = InputBox("請輸入第二個數字", "22", "5")
        Dim n, n1 As Long
        Dim b, t As Long
        Dim Cmp() As Long
        Dim ans As Integer
        n = 2 * 3 * 5 * 7
        n1 = 2 * 3 * 5 * 11
        If n > n1 Then
            b = n
            t = n1
        Else
            b = n1
            t = n
        End If
        ReDim Cmp(0)
        Do
            ReDim Preserve Cmp(UBound(Cmp) + 1)
            Cmp(UBound(Cmp)) = b Mod t

            If Cmp(UBound(Cmp)) = 0 Then
                Exit Do
            Else
                b = t
                t = Cmp(UBound(Cmp))
            End If
            ans = (n * n1) / t
        Loop
        MsgBox("最大公因數=", t)
        MsgBox("最小公倍數 = ", ans)
    End Sub '*

    Private Sub Button23_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button23.Click
        Dim ans As String

    End Sub
End Class
