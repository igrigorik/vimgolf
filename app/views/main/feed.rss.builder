xml.instruct! :xml, :version => "1.0"
xml.rss :version => "2.0" do
  xml.channel do
    xml.title "VimGolf: Latest Challenges"
    xml.description "Real Vim ninjas count every keystroke - do you?"
    xml.link "http://www.vimgolf.com/"

    @challenges.each do |challenge|
      xml.item do
        xml.title challenge.title
        xml.description challenge.description
        xml.pubDate challenge.created_at.to_s(:rfc822)
        xml.link challenge_url(challenge)
        xml.guid challenge_url(challenge)
      end
    end
  end
end