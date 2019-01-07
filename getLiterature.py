"""
Copyright (c) 2019-Jan-05 Zhijun Zhang
"""

import requests
from bs4 import BeautifulSoup
import json
import re
import os
import time
import random

def downloadHtmlFile(url, local):
    """download html file, save to local
    : type str, url
    : type str, local
    : rtype bool
    """
    time.sleep(random.randint(1,10))    
    response = requests.get(url)
    if (response.status_code < 400):
        with open(local, "wb") as fh:
            fh.write(response.content)
        print (url + "\n is saved to " + local + "\n")
        return True
    else:
        print ("Error: failed to download " + url + "\n")
    return False

def getPages():
    """download pages for the latest articles on Molecular Physics
    local files are "P1.html, P2.html, ..."
    : rtype bool
    """
    prefix = "https://www.tandfonline.com"
    url = prefix + "/action/showAxaArticles?journalCode=tmph20"
    local = "P1.html"
    if (not downloadHtmlFile(url, local)):
        return False
    else:
        soup = BeautifulSoup(open(local), "lxml")
        pages = soup.select(".pageLinks")
        if (len(pages) > 1): 
            cnt = 1
            for page in pages:
                a=page.find("a")
                if (a):
                    cnt += 1
                    url = prefix + a['href']
                    local = "P" + str(cnt) + ".html"
                    if (not downloadHtmlFile(url, local)):
                        return False
            if (cnt == 1):
                return False
        return True
    return False


def parsePage(fname):
    """ parse the local html page
    : type str, fname
    : rtype list of dict, infos
    """
    prefix = "https://www.tandfonline.com"
    soup = BeautifulSoup(open(fname), "lxml")
    articles = soup.select(".articleEntry")
    infos = []
    for article in articles:
        a = article.find("a", class_="ref nowrap")
        link = prefix + a['href']
        title = a.text
        try:
            author = article.find("div", class_="articleEntryAuthor").text
        except AttributeError:
            continue
        pubdate = article.find("div", class_="tocEPubDate").text
        img = article.find("img", src=True)
        if (not img):
            continue
        d = json.loads(img["data-src"])
        imgurl = prefix + d["src"]
        imgname = d["src"].split("/")[-1]
        infos.append({"title":title, 
            "url":link,
            "author":author,
            "pubdate":pubdate, 
            "imgurl":imgurl, 
            "imgname":imgname})
    return infos

def writePageInfo(date):
    """write information to file, in markdown format
    : type str, date
    : rtype
    >>> writeInfo("20190107")
    """
    path = os.getcwd()
    fname = date + ".md"
    fh = open(fname, "w")
    fh.write("---\n")
    fh.write("title: latest articles " + date + "\n")
    fh.write("...\n\n")
    for file in os.listdir(path):
        if re.search(r"P\d+.html", file):
            infos = parsePage(file)
            for info in infos:
                fh.write("# " + info["title"] + "\n\n")
                fh.write("<" + info["url"] + ">\n\n")
                fh.write(info["author"] + "\n\n")
                fh.write(info["pubdate"] + "\n\n") 
                fh.write("![](" + info["imgurl"] + ")\n\n")
                imglocal = "img/" + info["imgname"]
                if (not os.path.isfile(os.path.join(path, imglocal))):
                    downloadHtmlFile(info["imgurl"], imglocal)
    fh.close()
    return


if __name__ == "__main__":
    import sys
    if len (sys.argv) != 2 :
        print ("Usage: python getLiterature.py [flag]" )
        print ("       flag = 0: download pages and parse")
        print ("            = 1: parse local pages")
        sys.exit (1)
    havelocal = (sys.argv[1] == "1")
    date = time.strftime("%Y%m%d", time.localtime())
    if (not havelocal):
        getPages()
    writePageInfo(date)
