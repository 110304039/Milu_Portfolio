<template>
  <div>
    <div class="hero min-h-[80vh] bg-base-200">
      <div class="w-5/6 hero-content flex-col lg:flex-row">
        <div class="px-6 w-full lg:w-1/3">
          <h1 class="text-5xl font-bold py-6 h-1/3 text-center"><span class="text-primary">Ch.a.troom</span></h1>
          <p class="md-6 text-natural h-2/3 text-center"> Start to chat with your friends!</p>
          <!-- ========== table component =========== -->
          <div class="h-auto overflow-y-auto w-full mt-4 bg-white rounded-lg py-2">
            <table class="table table-auto w-full whitespace-normal">
              <!-- head -->
              <tbody>
                <!-- row 1 -->
                <tr>
                  <td>
                    <div class="flex-row mx-2">
                        <div class="flex items-center space-x-3">
                          <div class="font-bold cursor-pointer" >David</div>
                        </div>
                    </div>
                  </td>
                  <th class="text-right">
                    <p class="mx-4 text-warning">New</p>
                  </th>
                </tr>
                <!-- row end -->
                <tr>
                  <td>
                    <div class="flex-row mx-2">
                        <div class="flex items-center space-x-3">
                          <div class="font-bold cursor-pointer" >Lisa</div>
                        </div>
                    </div>
                  </td>
                  <th class="text-right">
                    <p class="mx-4 text-gray-400 font-normal">I got it. </p>
                  </th>
                </tr>
                <!-- row end -->
                <tr class="bg-secondary">
                  <td class="bg-secondary">
                    <div class="flex-row mx-2">
                        <div class="flex items-center space-x-3">
                          <div class="font-bold cursor-pointer" >Joe</div>
                        </div>
                    </div>
                  </td>
                  <th class="text-right bg-secondary">
                    <p class="mx-4 text-gray-400 font-normal">Sure, it w...</p>
                  </th>
                </tr>
                <!-- row end -->
                <tr>
                  <td>
                    <div class="flex-row mx-2">
                        <div class="flex items-center space-x-3">
                          <div class="font-bold cursor-pointer" >Betty</div>
                        </div>
                    </div>
                  </td>
                  <th class="text-right">
                    <p class="mx-4 text-gray-400 font-normal">Let's meet...</p>
                  </th>
                </tr>
                <!-- row end -->
                <tr>
                  <td>
                    <div class="flex-row mx-2">
                        <div class="flex items-center space-x-3">
                          <div class="font-bold cursor-pointer" >Andy</div>
                        </div>
                    </div>
                  </td>
                  <th class="text-right">
                    <p class="mx-4 text-gray-400 font-normal">OK! I got it.</p>
                  </th>
                </tr>
                <!-- row end -->
              </tbody>
            </table>
          </div>
        </div>
        <!-- left component -->
        <div class="px-6 w-full lg:w-2/3 bg-white rounded-2xl">
            <!-- ========== info component ========== -->
            <div class="p-6">
                <div class="chat chat-start py-4">
                    <div class="chat-image avatar">
                        <div class="w-10 rounded-full">
                        <img src="../assets/ChatRoom/info.jpeg" />
                        </div>
                    </div>
                    <div class="chat-bubble bg-primary text-black">Hi, where should we meet?</div>
                </div>
                <div class="chat chat-end">
                    <div class="chat-bubble bg-secondary text-black">How about the hotpot place near the station?</div>
                </div>
                <div class="chat chat-start py-4">
                    <div class="chat-image avatar">
                        <div class="w-10 rounded-full">
                        <img src="../assets/ChatRoom/info.jpeg" />
                        </div>
                    </div>
                    <div class="chat-bubble bg-primary text-black">Sounds good! What time should we meet?</div>
                </div>
                <div class="chat chat-end">
                    <div class="chat-bubble bg-secondary text-black">Let's meet at 5:50 PM.</div>
                </div>
                <div class="chat chat-start py-4">
                    <div class="chat-image avatar">
                        <div class="w-10 rounded-full">
                        <img src="../assets/ChatRoom/info.jpeg"/>
                        </div>
                    </div>
                    <div class="chat-bubble bg-primary text-black">Sure, it works for me.</div>
                </div>
                <div class="flex flex-row my-6 justify-between">
                    <input type="text" placeholder="Type here" class="input flex input-bordered input-primary w-full rounded-3xl" />
                    <!-- button with a arror point to left -->
                    <button class="flex justify-end btn btn-circle btn-ghost text-center hover:bg-primary hover:text-white mx-2" ><p class="px-4">&rarr;</p></button>
                </div>
            </div>
            <!-- ========== info component end ========== -->
        </div>
      </div>
    </div>
  </div>
</template>
<script>
export default {
  name: 'ChatRoom',
  data () {
    return {

    }
  },
  mounted () {
  },
  methods: {
  }
}
</script>
<style></style>
