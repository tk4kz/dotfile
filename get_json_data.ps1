
#$Proxy=New-object System.Net.WebProxy
#$WebSession=new-object Microsoft.PowerShell.Commands.WebRequestSession
#$WebSession.Proxy=$Proxy
#$Antwort=Invoke-RestMethod -Method Post -Uri "https://thisismyrestsite" -Body $BodyJson -WebSession $WebSession

# アメダステーブルデータ
$URL = 'https://www.jma.go.jp/bosai/amedas/const/amedastable.json'

# JSONデータ取得
$json_data = Invoke-RestMethod -Method Get -Uri $URL -SessionVariable loginsession

# 出力
#Write-Host $json_data
$loginsession | Get-Member | Out-Host

#{
#    "51331": {
#      "type": "C",
#      "elems": "11112010",
#      "lat": [34, 45.0],
#      "lon": [137, 20.5],
#      "alt": 3,
#      "kjName": "豊橋",
#      "knName": "トヨハシ",
#      "enName": "Toyohashi"
#    }
#  }